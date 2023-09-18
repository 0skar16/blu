use crate::{
    parse,
    parser::ast::{BluIterator, LetTarget, LoopOp, Operation, Statement, TableIndex, AST},
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
        Statement::Let(targets, source) => {
            let mut ids = vec![];
            for target in targets {
                match target {
                    LetTarget::ID(id) => ids.push(id),
                    LetTarget::Unwrap(_) => {
                        panic!("Lua doesn't have unwraps, use the simplifier before compiling")
                    }
                }
            }
            buf.push_str("local ");
            buf.push_str(&ids.join(", "));
            if let Some(source) = source {
                buf.push_str(" = ");
                buf.push_str(&to_lua(*source, ind, false));
            }
            buf.push('\n');
        }
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
                BluIterator::Each(_, _) => {
                    panic!("Lua doesn't have each iterators, use the simplifier before compiling")
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
        Statement::Import(_, _) => {
            panic!("Lua doesn't have an import statement, use the simplifier before compiling")
        }
        Statement::Export(source, mut target) => {
            if target == "default".into() {
                target = "__default".into();
            }
            buf.push_str(&to_lua(
                Statement::Assignment(Box::new(parse!("__export.{target}")), source),
                ind,
                do_ind,
            ))
        }
        Statement::Match(_, _, _, _) => {
            panic!("Lua doesn't have a match statement, use the simplifier before compiling")
        }
    }
    buf
}
