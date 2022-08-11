use std::{env, fs, process::ExitCode};

use codegen::Codegen;
use error::Error;
use inkwell::{context::Context, passes::PassManager, OptimizationLevel};
use lexer::Lexer;
use parser::Parser;

use crate::linter::Linter;

mod ast;
mod codegen;
mod error;
mod lexer;
mod linter;
mod parser;
mod tokens;
mod utils;

fn compile(filename: &str, content: &str) -> Result<(), Error> {
    let mut lexer = Lexer::new(content.to_string());
    let tokens = lexer.lex()?;
    let mut parser = Parser::new(tokens);
    let mut ast = parser.parse()?;
    let mut linter = Linter::new();
    let warnings = linter.lint(&mut ast)?;

    for warning in warnings.iter() {
        eprintln!("{}", warning.show(content.as_bytes(), &filename).unwrap());
    }

    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module("main");
    let fpm = PassManager::create(&module);
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.add_gvn_pass();
    fpm.add_cfg_simplification_pass();
    fpm.add_basic_alias_analysis_pass();
    fpm.add_promote_memory_to_register_pass();
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.initialize();

    let mut codegen = Codegen {
        current_function: None,
        begin_block: None,
        break_block: None,
        continue_block: None,
        return_value: None,
        context: &context,
        builder: &builder,
        module: &module,
        fpm: &fpm,
        variables: vec![],
        scope: 0,
        need_br: false,
        need_loading: true,
    };

    codegen.codegen(&ast);
    module.print_to_file("out.ll").unwrap();

    let ee = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();
    let compiled_fn = unsafe { ee.get_function::<unsafe extern "C" fn()>("main") }.unwrap();
    unsafe {
        compiled_fn.call();
    }

    Ok(())
}

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    let filename = args.get(1).unwrap();
    let contents = fs::read_to_string(&filename).unwrap();
    if let Err(error) = compile(&filename, &contents) {
        eprintln!("{}", error.show(contents.as_bytes(), &filename).unwrap());
        ExitCode::from(1)
    } else {
        ExitCode::from(0)
    }
}
