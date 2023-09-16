use std::hash::{Hash, Hasher};

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
            Statement::Match(a, b, c, d) => Self::simplify_match(Statement::Match(a, b, c, d)),
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
            Statement::Assignment(a, b) => Statement::Assignment(a, Box::new(Self::simplify_statement(*b))),
            Statement::Export(a, b) => Statement::Export(Box::new(Self::simplify_statement(*a)), b),
            Statement::Loop(a, block) => Statement::Loop(Box::new(Self::simplify_statement(*a)), Self::simplify_block(block)),
            Statement::Call(a, b, c) => Statement::Call(Box::new(Self::simplify_statement(*a)), Self::simplify_block(Block(b)).0, c),
            Statement::Index(a, b) => Statement::Index(Box::new(Self::simplify_statement(*a)), Box::new(Self::simplify_statement(*b))),
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
                    Box::new(Statement::Get("pairs".to_string())),
                    vec![Self::simplify_statement(*b)],
                    false,
                )),
            ),
            BluIterator::Iterator(a, b) => {
                BluIterator::Iterator(a, Box::new(Self::simplify_statement(*b)))
            }
        }
    }
    fn simplify_match(st: Statement) -> Statement {
        match st {
            Statement::Match(input, cases, default_case, is_standalone) => {
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
                            vec![LetTarget::ID(format!("match_{match_hash:x}_{i}"))],
                            Some(Box::new(func)),
                        ));
                        lookup_table.extend(case_possibilites.into_iter().map(|lit| {
                            (
                                TableIndex::Literal(lit),
                                Statement::Get(format!("match_{match_hash:x}_{i}")),
                            )
                        }));
                    } else {
                        lookup_table
                            .push((TableIndex::Literal(case_possibilites[0].clone()), func));
                    }
                    i += 1;
                }
                block.push(Statement::Return(vec![Statement::Index(
                    Box::new(Statement::Paren(Box::new(Statement::Table(lookup_table)))),
                    Box::new(Statement::Get("input".to_string())),
                )]));
                let mut match_statement = Statement::Call(
                    Box::new(Statement::Paren(Box::new(Statement::Operation(
                        Box::new(Statement::Call(
                            Box::new(Statement::Paren(Box::new(Statement::Function(
                                None,
                                vec!["input".to_string()],
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
                                    pub enum SimplificationTarget {
                                        Lua,
                                    }
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
                        vec![LetTarget::ID("_".to_string())],
                        Some(Box::new(match_statement)),
                    );
                }
                match_statement
            }
            _ => unreachable!(),
        }
    }
}
