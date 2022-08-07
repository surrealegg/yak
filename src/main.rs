use std::{env, fs};

use lexer::Lexer;
use parser::Parser;

use crate::typecheker::Typecheker;

mod ast;
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
                        _ => {}
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
