use std::{env, fs};

use codegen::Codegen;
use inkwell::{context::Context, passes::PassManager, OptimizationLevel};
use lexer::Lexer;
use parser::Parser;

use crate::typecheker::Typecheker;

mod ast;
mod codegen;
mod error;
mod lexer;
mod parser;
mod tokens;
mod typecheker;
mod utils;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = args.get(1).unwrap();
    let content = fs::read_to_string(&filename).unwrap();
    let mut lexer = Lexer::new(content.clone());

    match lexer.lex() {
        Ok(tokens) => {
            let mut parser = Parser::new(tokens);
            match parser.parse() {
                Ok(ast) => {
                    let mut typecheker = Typecheker::new();
                    match typecheker.check_statmenets(&ast) {
                        Err(error) => {
                            eprintln!("{}", error.show(content.as_bytes(), &filename).unwrap());
                        }
                        Ok((changed_ast, warnings)) => {
                            for warning in warnings.iter() {
                                println!(
                                    "{}",
                                    warning.show(content.as_bytes(), &filename).unwrap()
                                );
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
                            };

                            codegen.statements(&changed_ast);
                            module.print_to_file("out.ll").unwrap();

                            let ee = module
                                .create_jit_execution_engine(OptimizationLevel::None)
                                .unwrap();
                            let compiled_fn =
                                unsafe { ee.get_function::<unsafe extern "C" fn()>("main") }
                                    .unwrap();
                            unsafe {
                                compiled_fn.call();
                            }
                        }
                    }
                }
                Err(error) => {
                    eprintln!("{}", error.show(content.as_bytes(), &filename).unwrap());
                }
            }
        }
        Err(error) => {
            eprintln!("{}", error.show(content.as_bytes(), &filename).unwrap());
        }
    }
}
