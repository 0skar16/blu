use std::{
    collections::HashMap,
    hash::{Hash, Hasher}, rc::Rc,
};

use fasthash::{murmur2::Hasher32, FastHasher};

use crate::{parse, parser::ast::*};

pub struct Simplifier;

impl Simplifier {
    pub fn simplify(target: SimplificationTarget, ast: AST) -> AST {
        match target {
            SimplificationTarget::Lua => LuaSimplifier::simplify_ast(ast),
        }
    }
}
pub enum SimplificationTarget {
    Lua,
}

pub struct LuaSimplifier;
impl LuaSimplifier {
    pub fn simplify_ast(ast: AST) -> AST {
        AST {
            statements: ast
                .statements
                .into_iter()
                .map(|st| LuaSimplifier::simplify_statement(st))
                .collect(),
        }
    }
    pub fn simplify_block(block: Block) -> Block {
        Block(
            block
                .0
                .into_iter()
                .map(|st| LuaSimplifier::simplify_statement(st))
                .collect(),
        )
    }
    pub fn simplify_statement(st: Statement) -> Statement {
        match st {
            Statement::Match(a, b, c, d) => Self::simplify_match(a, b, c, d),
            Statement::For(iter, block) => {
                Statement::For(Self::simplify_iterator(iter), Self::simplify_block(block))
            }
            Statement::Function(a, b, block, c) => Statement::Function(
                a.map(|st| Box::new(Self::simplify_statement(*st))),
                b,
                Self::simplify_block(block),
                c,
            ),
            Statement::While(a, block) => Statement::While(
                Box::new(Self::simplify_statement(*a)),
                Self::simplify_block(block),
            ),
            Statement::If(a, block, else_ifs, _else) => Statement::If(
                Box::new(Self::simplify_statement(*a)),
                Self::simplify_block(block),
                else_ifs
                    .into_iter()
                    .map(|(st, block)| (Self::simplify_statement(st), Self::simplify_block(block)))
                    .collect(),
                _else.map(|_else| Self::simplify_block(_else)),
            ),
            Statement::Paren(a) => Statement::Paren(Box::new(Self::simplify_statement(*a))),
            Statement::Assignment(a, b) => {
                Statement::Assignment(a, Box::new(Self::simplify_statement(*b)))
            }
            Statement::Export(a, b) => Statement::Export(Box::new(Self::simplify_statement(*a)), b),
            Statement::Loop(a, block) => Statement::Loop(
                Box::new(Self::simplify_statement(*a)),
                Self::simplify_block(block),
            ),
            Statement::Call(a, b, c) => Statement::Call(
                Box::new(Self::simplify_statement(*a)),
                Self::simplify_block(Block(b)).0,
                c,
            ),
            Statement::Index(a, b) => Statement::Index(
                Box::new(Self::simplify_statement(*a)),
                Box::new(Self::simplify_statement(*b)),
            ),
            Statement::Table(a) => Statement::Table(
                a.into_iter()
                    .map(|(index, st)| {
                        (
                            match index {
                                TableIndex::Statement(st) => {
                                    TableIndex::Statement(Self::simplify_statement(st))
                                }
                                i => i,
                            },
                            Self::simplify_statement(st),
                        )
                    })
                    .collect(),
            ),
            Statement::Let(targets, src) => Self::simplify_let(targets, src),
            Statement::Import(target, src) => Self::simplify_import(target, src),
            st => st,
        }
    }
    fn simplify_iterator(iter: BluIterator) -> BluIterator {
        match iter {
            BluIterator::Numerical(a, b, c, d) => BluIterator::Numerical(
                a,
                Box::new(Self::simplify_statement(*b)),
                Box::new(Self::simplify_statement(*c)),
                d.map(|d| Box::new(Self::simplify_statement(*d))),
            ),
            BluIterator::Each(a, b) => BluIterator::Iterator(
                a,
                Box::new(Statement::Call(
                    Box::new(Statement::Get("pairs".into())),
                    vec![Self::simplify_statement(*b)],
                    false,
                )),
            ),
            BluIterator::Iterator(a, b) => {
                BluIterator::Iterator(a, Box::new(Self::simplify_statement(*b)))
            }
        }
    }
    fn simplify_match(
        input: Box<Statement>,
        cases: Vec<(Vec<Literal>, MatchOutput)>,
        default_case: Option<MatchOutput>,
        is_standalone: bool,
    ) -> Statement {
        let mut hasher = Hasher32::new();
        input.hash(&mut hasher);
        let match_hash = hasher.finish();
        let mut lookup_table = vec![];
        let mut i = 0;
        let mut block = vec![];
        for (case_possibilites, output) in cases {
            let func = Statement::Function(
                None,
                vec![],
                match output {
                    MatchOutput::Block(block) => block,
                    MatchOutput::Statement(st) => Block(vec![Statement::Return(vec![*st])]),
                },
                false,
            );
            if case_possibilites.len() > 1 {
                block.push(Statement::Let(
                    vec![LetTarget::ID(format!("match_{match_hash:x}_{i}").into())],
                    Some(Box::new(func)),
                ));
                lookup_table.extend(case_possibilites.into_iter().map(|lit| {
                    (
                        TableIndex::Literal(lit),
                        Statement::Get(format!("match_{match_hash:x}_{i}").into()),
                    )
                }));
            } else {
                lookup_table.push((TableIndex::Literal(case_possibilites[0].clone()), func));
            }
            i += 1;
        }
        block.push(Statement::Return(vec![Statement::Index(
            Box::new(Statement::Paren(Box::new(Statement::Table(lookup_table)))),
            Box::new(Statement::Get("input".into())),
        )]));
        let mut match_statement = Statement::Call(
            Box::new(Statement::Paren(Box::new(Statement::Operation(
                Box::new(Statement::Call(
                    Box::new(Statement::Paren(Box::new(Statement::Function(
                        None,
                        vec!["input".into()],
                        Block(block),
                        false,
                    )))),
                    vec![*input],
                    false,
                )),
                Operation::Or,
                Some(Box::new(
                    default_case
                        .map(|output| {
                            Statement::Function(
                                None,
                                vec![],
                                match output {
                                    MatchOutput::Block(block) => block,
                                    MatchOutput::Statement(st) => {
                                        Block(vec![Statement::Return(vec![*st])])
                                    }
                                },
                                false,
                            )
                        })
                        .unwrap_or(parse!("fn(){{}}")),
                )),
            )))),
            vec![],
            true,
        );
        if is_standalone {
            match_statement = Statement::Let(
                vec![LetTarget::ID("_".into())],
                Some(Box::new(match_statement)),
            );
        }
        match_statement
    }
    fn simplify_import(target: ImportTarget, src: Rc<str>) -> Statement {
        match target {
            ImportTarget::Default(id) => Statement::Let(
                vec![LetTarget::ID(id)],
                Some(Box::new(Statement::Child(
                    Box::new(Statement::Call(
                        Box::new(Statement::Get("require".into())),
                        vec![Statement::Literal(Literal::String(src))],
                        false,
                    )),
                    Box::new(Statement::Get("__default".into())),
                ))),
            ),
            ImportTarget::Unwrap(unwrap) => Self::simplify_unwrap(
                unwrap,
                Statement::Call(
                    Box::new(Statement::Get("require".into())),
                    vec![Statement::Literal(Literal::String(src))],
                    false,
                ),
            ),
        }
    }
    fn simplify_let(targets: Vec<LetTarget>, src: Option<Box<Statement>>) -> Statement {
        if targets
            .iter()
            .find(|t| match t {
                LetTarget::Unwrap(_) => true,
                _ => false,
            })
            .is_none()
        {
            return Statement::Let(targets, src);
        }
        let mut x = 0;
        let mut blk = Block(vec![]);
        let mut names: HashMap<Rc<str>, Statement> = HashMap::new();
        let mut let_outputs = vec![];
        let mut hasher = Hasher32::new();
        src.hash(&mut hasher);
        let hash = hasher.finish();
        for target in targets {
            match target {
                LetTarget::ID(id) => {
                    let_outputs.push(id.clone());
                    names.insert(id.clone(), Statement::Get(id));
                }
                LetTarget::Unwrap(targets) => {
                    let name: Rc<str> = format!("unwrapped_{hash:x}_{x:x}").into();
                    let_outputs.push(name.clone());
                    for target in targets {
                        Self::resolve_unwrap_target(
                            target,
                            &mut names,
                            &mut blk,
                            Statement::Get(name.clone()),
                        );
                        x += 1;
                    }
                }
            }
        }
        let mut _blk = Block(vec![Statement::Let(
            let_outputs
                .into_iter()
                .map(|name| LetTarget::ID(name))
                .collect(),
            src,
        )]);
        _blk.extend(blk.0);
        _blk.push(Statement::Return(
            names.values().map(|ret| ret.clone()).collect(),
        ));
        Statement::Let(
            names
                .keys()
                .map(|name| LetTarget::ID(name.clone()))
                .collect(),
            Some(Box::new(Statement::Call(
                Box::new(Statement::Paren(Box::new(Statement::Function(
                    None,
                    vec![],
                    _blk,
                    false,
                )))),
                vec![],
                false,
            ))),
        )
    }
    fn simplify_unwrap(unwrap: Vec<UnwrapTarget>, src: Statement) -> Statement {
        let mut blk = Block(vec![]);
        let mut names: HashMap<Rc<str>, Statement> = HashMap::new();
        for target in unwrap {
            Self::resolve_unwrap_target(target, &mut names, &mut blk, src.clone());
        }
        blk.push(Statement::Return(
            names.values().map(|ret| ret.clone()).collect(),
        ));
        Statement::Let(
            names
                .keys()
                .map(|name| LetTarget::ID(name.clone()))
                .collect(),
            Some(Box::new(Statement::Call(
                Box::new(Statement::Paren(Box::new(Statement::Function(
                    None,
                    vec!["src".into()],
                    blk,
                    false,
                )))),
                vec![src],
                false,
            ))),
        )
    }
    fn resolve_unwrap_target(
        target: UnwrapTarget,
        names: &mut HashMap<Rc<str>, Statement>,
        blk: &mut Block,
        src: Statement,
    ) {
        match target {
            UnwrapTarget::ID(id) => {
                names.insert(
                    id.clone(),
                    Statement::Child(Box::new(src), Box::new(Statement::Get(id))),
                );
            }
            UnwrapTarget::Unwrap(targets, id) => {
                for target in targets {
                    Self::resolve_unwrap_target(
                        target,
                        names,
                        blk,
                        Statement::Child(
                            Box::new(src.clone()),
                            Box::new(Statement::Get(id.clone())),
                        ),
                    )
                }
            }
            UnwrapTarget::ReassignID(id, new_id) => {
                names.insert(
                    new_id,
                    Statement::Child(Box::new(src), Box::new(Statement::Get(id))),
                );
            }
        }
    }
}
