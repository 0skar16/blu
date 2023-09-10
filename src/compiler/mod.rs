use std::hash::{Hash, Hasher};

use fasthash::{murmur2::Hasher32, FastHasher};

use crate::{
    parse, parse_standalone,
    parser::ast::{
        Block, BluIterator, ImportTarget, LetTarget, LoopOp, MatchOutput, Operation, Statement,
        TableIndex, UnwrapTarget, AST,
    },
};

pub fn compile(ast: AST) -> String {
    let mut buf = String::new();
    buf.push_str("local __export = {}\n");
    for statement in ast.statements {
        buf.push_str(&to_lua(statement, 0, true))
    }
    buf.push_str("return __export");
    buf
}
fn to_lua(statement: Statement, ind: u8, do_ind: bool) -> String {
    let mut buf = String::new();
    if do_ind {
        buf.push_str(&"\t".repeat(ind as usize));
    }
    match statement {
        Statement::Call(called, args, standalone) => {
            buf.push_str(&to_lua(*called, ind, false));
            buf.push('(');
            let s_args: Vec<String> = args
                .into_iter()
                .map(|arg| to_lua(arg, ind, false))
                .collect();
            buf.push_str(&s_args.join(", "));
            buf.push_str(")");
            if standalone {
                buf.push('\n');
            }
        }
        Statement::Get(name) => buf.push_str(&name),
        Statement::Child(parent, child) => {
            buf.push_str(&to_lua(*parent, ind, false));
            buf.push('.');
            buf.push_str(&to_lua(*child, ind, false));
        }
        Statement::Let(target, source) => match target {
            LetTarget::ID(name) => {
                buf.push_str("local ");
                buf.push_str(&name);
                if let Some(source) = source {
                    buf.push_str(" = ");
                    buf.push_str(&to_lua(*source, ind, false));
                }
                buf.push('\n');
            }
            LetTarget::Unwrap(targets) => buf.push_str(&resolve_unwrap(
                targets,
                *source.expect("Source of unwrap must be some"),
                ind,
                do_ind,
            )),
        },
        Statement::Global(name, source) => {
            buf.push_str(&name);
            buf.push_str(" = ");
            if let Some(source) = source {
                buf.push_str(&to_lua(*source, ind, false));
            } else {
                buf.push_str("nil");
            }
            buf.push('\n');
        }
        Statement::Assignment(target, source) => {
            buf.push_str(&to_lua(*target, ind, false));
            buf.push_str(" = ");
            buf.push_str(&to_lua(*source, ind, false));
            buf.push('\n');
        }
        Statement::Operation(operand, operation, operator) => {
            if operator.is_some() {
                buf.push_str(&to_lua(*operand.clone(), ind, false));
                buf.push(' ');
            }
            buf.push_str(&format!(
                "{} ",
                match operation {
                    Operation::Add => "+",
                    Operation::Sub => "-",
                    Operation::Div => "/",
                    Operation::Mul => "*",
                    Operation::Mod => "%",
                    Operation::And => "and",
                    Operation::Or => "or",
                    Operation::Equal => "==",
                    Operation::NotEqual => "!=",
                    Operation::Greater => ">",
                    Operation::Lesser => "<",
                    Operation::GE => ">=",
                    Operation::LE => "<=",
                    Operation::Exp => "^",
                    Operation::Not => "not",
                    Operation::StringAdd => "..",
                    Operation::Len => "#",
                    Operation::Arrow => "=>",
                }
            ));
            if let Some(operator) = operator {
                buf.push_str(&to_lua(*operator, ind, false))
            } else {
                buf.push_str(&to_lua(*operand, ind, false))
            }
        }
        Statement::Comment(_, _) => {}
        Statement::Literal(lit) => buf.push_str(&match lit {
            crate::parser::ast::Literal::Number(number) => number.to_string(),
            crate::parser::ast::Literal::String(s) => format!("\"{}\"", s.replace("\"", "\\\"")),
            crate::parser::ast::Literal::Boolean(bool) => bool.to_string(),
            crate::parser::ast::Literal::Slice(slice) => {
                let s_slice: Vec<String> = slice
                    .into_iter()
                    .map(|arg| to_lua(arg, ind, false))
                    .collect();

                return format!("{{{}}}", s_slice.join(", "));
            }
        }),
        Statement::Return(ret) => {
            buf.push_str(&format!(
                "return {}\n",
                ret.into_iter()
                    .map(|s| to_lua(s, ind, false))
                    .collect::<Vec<_>>()
                    .join(", ")
            ));
        }
        Statement::Table(entries) => {
            buf.push_str("{");
            if entries.len() > 0 {
                buf.push('\n');
            }
            let len = entries.len();
            let mut i = 0;
            for (index, entry) in entries {
                buf.push_str(&"\t".repeat(ind as usize + 1));
                match index {
                    TableIndex::None => {}
                    TableIndex::Ident(name) => {
                        buf.push_str(&name);
                        buf.push_str(" = ");
                    }
                    TableIndex::Literal(index) => {
                        buf.push('[');
                        buf.push_str(&to_lua(Statement::Literal(index), ind + 1, false));
                        buf.push_str("] = ");
                    }
                    TableIndex::Statement(index) => {
                        buf.push('[');
                        buf.push_str(&to_lua(index, ind + 1, false));
                        buf.push_str("] = ");
                    }
                }
                buf.push_str(&to_lua(entry, ind + 1, false));
                i += 1;
                if i != len {
                    buf.push(',');
                }
                buf.push('\n');
            }
            buf.push_str(&"\t".repeat(ind as usize));
            buf.push('}');
        }
        Statement::Function(name, args, block, standalone) => {
            buf.push_str("function");
            if let Some(name) = name {
                buf.push_str(" ");
                buf.push_str(&to_lua(*name, ind + 1, false));
            }
            buf.push('(');
            buf.push_str(&args.join(", "));
            buf.push_str(")\n");
            for stmt in block.0 {
                buf.push_str(&to_lua(stmt, ind + 1, true));
            }

            buf.push_str(&"\t".repeat(ind as usize));
            buf.push_str("end");
            if standalone {
                buf.push('\n');
            }
        }
        Statement::For(iter, block) => {
            buf.push_str("for ");
            match iter {
                BluIterator::Numerical(iterative, init, end, step) => {
                    buf.push_str(&format!(
                        "{} = {}, {}",
                        iterative,
                        to_lua(*init, ind, false),
                        to_lua(*end, ind, false)
                    ));
                    if let Some(step) = step {
                        buf.push_str(&format!(", {}", to_lua(*step, ind, false)));
                    }
                }
                BluIterator::Each(iterative, iterator) => {
                    buf.push_str(&format!(
                        "{} in pairs({})",
                        iterative.join(", "),
                        to_lua(*iterator, ind + 1, false)
                    ));
                }
                BluIterator::Iterator(iterative, iterator) => {
                    buf.push_str(&format!(
                        "{} in {}",
                        iterative.join(", "),
                        to_lua(*iterator, ind + 1, false)
                    ));
                }
            }
            buf.push_str(" do\n");
            for stmt in block.0 {
                buf.push_str(&to_lua(stmt, ind + 1, true));
            }
            buf.push_str(&"\t".repeat(ind as usize));
            buf.push_str("end\n");
        }
        Statement::While(stmt, block) => {
            buf.push_str(&format!("while ({}) do\n", to_lua(*stmt, ind + 1, false)));
            for stmt in block.0 {
                buf.push_str(&to_lua(stmt, ind + 1, true));
            }
            buf.push_str(&"\t".repeat(ind as usize));
            buf.push_str("end\n");
        }
        Statement::If(stmt, block, else_ifs, _else) => {
            buf.push_str("if(");
            buf.push_str(&to_lua(*stmt, ind, false));
            buf.push_str(") then\n");
            for stmt in block.0 {
                buf.push_str(&to_lua(stmt, ind + 1, true));
            }
            for (stmt, block) in else_ifs {
                buf.push_str(&"\t".repeat(ind as usize));
                buf.push_str("elseif(");
                buf.push_str(&to_lua(stmt, ind, false));
                buf.push_str(") then\n");
                for stmt in block.0 {
                    buf.push_str(&to_lua(stmt, ind + 1, true));
                }
            }
            if let Some(block) = _else {
                buf.push_str(&"\t".repeat(ind as usize));
                buf.push_str("else\n");
                for stmt in block.0 {
                    buf.push_str(&to_lua(stmt, ind + 1, true));
                }
            }
            buf.push_str(&"\t".repeat(ind as usize));
            buf.push_str("end\n");
        }
        Statement::Paren(stmt) => {
            buf.push_str("(");
            buf.push_str(&to_lua(*stmt, ind, false));
            buf.push_str(")");
        }
        Statement::Index(indexed, indexer) => {
            buf.push_str(&to_lua(*indexed, ind, false));
            buf.push('[');
            buf.push_str(&to_lua(*indexer, ind, false));
            buf.push(']');
        }
        Statement::Nil => buf.push_str("nil"),
        Statement::Loop(stmt, block) => {
            buf.push_str("repeat\n");
            for stmt in block.0 {
                buf.push_str(&to_lua(stmt, ind + 1, true));
            }
            buf.push_str(&"\t".repeat(ind as usize));
            buf.push_str(&format!("until({})\n", to_lua(*stmt, ind + 1, false)));
        }
        Statement::LoopOperation(op) => match op {
            LoopOp::Break => buf.push_str("break\n"),
            LoopOp::Continue => buf.push_str("continue\n"),
        },
        Statement::Method(parent, method) => {
            buf.push_str(&format!("{}:{}", to_lua(*parent, ind, false), method));
        }
        Statement::Import(target, source) => match target {
            ImportTarget::Default(id) => buf.push_str(&to_lua(
                parse_standalone!("let {id} = require(\"{source}\").__default;"),
                ind,
                do_ind,
            )),
            ImportTarget::Unwrap(unwrap) => buf.push_str(&resolve_unwrap(
                unwrap,
                parse!("require(\"{source}\")"),
                ind,
                do_ind,
            )),
        },
        Statement::Export(source, mut target) => {
            if target == "default".to_string() {
                target = "__default".to_string();
            }
            buf.push_str(&to_lua(
                Statement::Assignment(Box::new(parse!("__export.{target}")), source),
                ind,
                do_ind,
            ))
        }
        Statement::Match(input, cases, default_case) => {
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
                        LetTarget::ID(format!("match_{match_hash:x}_{i}")),
                        Some(Box::new(func)),
                    ));
                    lookup_table.extend(case_possibilites.into_iter().map(|lit| {
                        (
                            TableIndex::Literal(lit),
                            Statement::Get(format!("match_{match_hash:x}_{i}")),
                        )
                    }));
                } else {
                    lookup_table.push((TableIndex::Literal(case_possibilites[0].clone()), func));
                }
                i += 1;
            }
            block.push(Statement::Return(vec![Statement::Index(
                Box::new(Statement::Paren(Box::new(Statement::Table(lookup_table)))),
                Box::new(Statement::Get("input".to_string())),
            )]));
            let match_statement = Statement::Let(
                LetTarget::ID("_".to_string()),
                Some(Box::new(Statement::Call(
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
                ))),
            );
            buf.push_str(&to_lua(match_statement, ind, false));
        }
    }
    buf
}
fn resolve_unwrap(unwrap: Vec<UnwrapTarget>, owner: Statement, ind: u8, do_ind: bool) -> String {
    let mut buf = String::new();
    let mut hasher = Hasher32::new();
    owner.hash(&mut hasher);
    let unwrapped_object = format!("unwrapped_object_{:x}", hasher.finish());
    buf.push_str(&to_lua(
        Statement::Let(
            LetTarget::ID(unwrapped_object.clone()),
            Some(Box::new(owner)),
        ),
        ind,
        do_ind,
    ));
    for target in unwrap {
        match target {
            //UnwrapTarget::ID(id) => buf.push_str(&to_lua(Statement::Let(LetTarget::ID(id.clone()), Some(Box::new(Statement::Child(Box::new(Statement::Get(unwrapped_object.clone())), Box::new(Statement::Get(id)))))), ind, do_ind)),
            UnwrapTarget::ID(id) => buf.push_str(&to_lua(
                parse_standalone!("let {id} = {unwrapped_object}.{id};"),
                ind,
                do_ind,
            )),
            UnwrapTarget::Unwrap(targets, id) => buf.push_str(&resolve_unwrap(
                targets,
                parse!("{unwrapped_object}.{id}"),
                ind,
                do_ind,
            )),
            //UnwrapTarget::Default(id) => buf.push_str(&to_lua(Statement::Let(LetTarget::ID(id), Some(Box::new(Statement::Child(Box::new(Statement::Get(unwrapped_object.clone())), Box::new(Statement::Get("__default".to_string())))))), ind, do_ind)),
            UnwrapTarget::Default(id) => buf.push_str(&to_lua(
                parse_standalone!("let {id} = {unwrapped_object}.__default;"),
                ind,
                do_ind,
            )),
        }
    }
    buf
}
