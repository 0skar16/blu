extern crate test;

#[cfg(test)]
mod tests {
    use crate::{
        compiler::Compiler, lexer::Lexer, optimizer::simplifier::Simplifier, parser::Parser, Target,
    };

    use super::test::Bencher;

    #[bench]
    fn bench(b: &mut Bencher) {
        let code = std::fs::read_to_string("examples/basic.blu").unwrap();
        b.iter(move || {
            let token_stream = Lexer::new(code.chars()).tokenize().unwrap();
            let ast = Parser::new(token_stream).parse().unwrap();
            let ast = Simplifier::simplify(Target::Lua, ast);
            let _ = Compiler::compile(Target::Lua, ast);
        });
    }
}
