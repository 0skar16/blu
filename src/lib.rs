#![feature(let_chains)]
#![feature(test)]

use clap::{builder::PossibleValue, ValueEnum};

pub mod compiler;
pub mod lexer;
pub mod optimizer;
pub mod parser;
pub mod tests;

#[derive(Clone, Debug, Copy)]
pub enum Target {
    Lua,
}

impl ValueEnum for Target {
    fn value_variants<'a>() -> &'a [Self] {
        &[Target::Lua]
    }

    fn to_possible_value(&self) -> Option<clap::builder::PossibleValue> {
        match self {
            Target::Lua => Some(PossibleValue::new("lua")),
        }
    }
}
