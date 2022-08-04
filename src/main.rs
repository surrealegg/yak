use std::{env, fs};

use lexer::Lexer;
use parser::Parser;

mod ast;
mod error;
mod lexer;
mod parser;
mod tokens;
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
                    for item in ast.iter() {
                        let line = item.span().show(content.as_bytes(), "\u{001b}[33m");
                        println!("{:#?}", item);
                        println!("{}", line.unwrap());
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
