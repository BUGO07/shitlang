use std::time::Instant;

use crate::{lexer::Lexer, syntax::AbstractSyntaxTree};

mod lexer;
mod syntax;
mod token;

fn main() -> anyhow::Result<()> {
    let mut lexer = Lexer::new(std::fs::read_to_string("res/code.shit")?);

    let time = Instant::now();
    lexer.tokenize()?;
    println!("Lexing took {:?}", time.elapsed());

    dbg!(lexer.tokens());

    let _ = AbstractSyntaxTree::new(lexer.tokens().clone()).build()?;

    Ok(())
}
