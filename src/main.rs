use inkwell::context::Context;

use crate::{
    codegen::CodeGen, interpreter::ControlFlow, lexer::Lexer, parser::Parser, sema::SymbolTable,
};

mod codegen;
mod interpreter;
mod lexer;
mod native_functions;
mod parser;
mod sema;
mod token;

#[cfg(test)]
mod tests;

fn main() -> anyhow::Result<()> {
    let args = std::env::args().collect::<Vec<_>>();
    if args.len() != 2 {
        anyhow::bail!("Usage: {} <source_file>", args[0]);
    }

    run_file(&args[1])?;

    Ok(())
}

fn run_file(name: &str) -> anyhow::Result<ControlFlow> {
    let mut lexer = Lexer::new(std::fs::read_to_string(name)?);
    lexer
        .tokenize()
        .map_err(|e| anyhow::anyhow!("Lexer: {e} at {:?}", lexer.location()))?;

    let mut parser = Parser::new(lexer.tokens().clone());
    parser.parse().map_err(|e| {
        anyhow::anyhow!(
            "Parser: {e} at token {:?}",
            parser.tokens[parser.current_index]
        )
    })?;

    // dbg!(&parser.global_scope);

    let mut sym_table = SymbolTable::new();
    sym_table
        .build(&parser.global_scope)
        .map_err(|e| anyhow::anyhow!("Semantic: {e}"))?;

    let context = Context::create();
    let mut codegen = CodeGen::new(&context);
    codegen
        .generate(&parser.global_scope)
        .map_err(|e| anyhow::anyhow!("CodeGen: {e}"))?;

    Ok(ControlFlow::None)
    // let mut interpreter = Interpreter::new();
    // interpreter
    //     .interpret(&parser.global_scope)
    //     .map_err(|e| anyhow::anyhow!("Interpreter: {e}"))
}
