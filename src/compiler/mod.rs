use crate::{parser::ast::AST, Target};

pub mod lua;

pub struct Compiler;

impl Compiler {
    pub fn compile(target: Target, ast: AST) -> String {
        match target {
            Target::Lua => lua::compile(ast),
        }
    }
}
