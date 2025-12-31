use std::{path::Path, process::Command};

use inkwell::{
    OptimizationLevel,
    context::Context,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
};

use crate::{codegen::CodeGen, lexer::Lexer, parser::Parser, sema::SymbolTable};

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

fn run_file(name: &str) -> anyhow::Result<()> {
    let mut lexer = Lexer::new(std::fs::read_to_string(name)?);
    lexer
        .tokenize()
        .map_err(|e| anyhow::anyhow!("Lexer: {e} at {:?}", lexer.location()))?;

    let mut parser = Parser::new(lexer.tokens().clone());
    parser
        .parse()
        .map_err(|e| anyhow::anyhow!("Parser: {e} at {:?}", parser.tokens[parser.current_index]))?;

    // dbg!(&parser.global_scope);

    let mut sym_table = SymbolTable::new();
    sym_table
        .build(&parser.global_scope)
        .map_err(|e| anyhow::anyhow!("Semantic: {e}"))?;

    let context = Context::create();
    let mut codegen = CodeGen::new(&context);
    let module = codegen
        .generate(&parser.global_scope)
        .map_err(|e| anyhow::anyhow!("CodeGen: {e}"))?;

    let name = Path::new(name).file_name().unwrap().to_str().unwrap();
    let build_path = Path::new("build");
    if !build_path.exists() {
        std::fs::create_dir(build_path)?;
    }
    module
        .print_to_file(build_path.join(name).with_extension("ll"))
        .unwrap();

    Target::initialize_native(&InitializationConfig::default()).unwrap();
    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple).unwrap();
    let cpu = "generic";
    let features = "";

    let target_machine = target
        .create_target_machine(
            &triple,
            cpu,
            features,
            OptimizationLevel::Default,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .unwrap();

    target_machine
        .write_to_file(
            &module,
            FileType::Object,
            &build_path.join(name).with_extension("o"),
        )
        .unwrap();

    Command::new("clang")
        .args([
            build_path
                .join(name)
                .with_extension("o")
                .to_string_lossy()
                .as_ref(),
            "-o",
            build_path
                .join(name)
                .with_extension("")
                .to_string_lossy()
                .as_ref(),
        ])
        .status()
        .map_err(|e| anyhow::anyhow!("Failed to execute clang: {}", e))?;

    #[cfg(not(test))]
    Command::new(build_path.join(name).with_extension("").to_str().unwrap()).status()?;

    Ok(())
    // let mut interpreter = Interpreter::new();
    // interpreter
    //     .interpret(&parser.global_scope)
    //     .map_err(|e| anyhow::anyhow!("Interpreter: {e}"))
}
