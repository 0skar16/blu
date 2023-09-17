extern crate test;

#[cfg(test)]
mod tests {
    use crate::{compiler::compile, optimizer::simplifier::{SimplificationTarget, Simplifier}, lexer::Lexer, parser::Parser};

    use super::test::Bencher;

    #[bench]
    fn bench(b: &mut Bencher) {
        let code = std::fs::read_to_string("examples/basic.blu").unwrap();
        b.iter(move || {
            let token_stream = Lexer::new(code.chars()).tokenize().unwrap();
            let ast = Parser::new(token_stream).parse().unwrap();
            let ast = Simplifier::simplify(SimplificationTarget::Lua, ast);
            let _ = compile(ast);
        });
    }
}