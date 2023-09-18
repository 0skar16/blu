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

#[macro_export]
macro_rules! time {
    ($expr:expr, $name:expr) => {{
        let start = std::time::Instant::now();
        let expr = { $expr };
        println!(
            "{}: {} μs",
            $name,
            (std::time::Instant::now() - start).as_micros()
        );
        expr
    }};
}
