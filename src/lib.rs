#![feature(let_chains)]
pub mod compiler;
pub mod lexer;
pub mod parser;
#[macro_export]
macro_rules! time {
    ($expr:expr, $name:expr) => {
        {
            let start = std::time::Instant::now();
            let expr = {$expr};
            println!("{}: {} μs", $name, (std::time::Instant::now()-start).as_micros());
            expr
        }
    };
}