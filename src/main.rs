mod lexer;
mod parser;
mod codegen;
mod fileio;

use lexer::*;
use parser::*;
use codegen::*;
use fileio::*;

fn main() {
    let filename = "test.beeasm";
    let program = match fileio::read_to_string(filename) {
        Ok(s) => s,
        Err(e) => {
            println!("{}", e);
            return;
        }
    };

    let tokens = match Lexer::new(&program).tokenize() {
        Ok(vec) => {
            //println!("{:#?}", vec);
            vec
        }
        Err(e) => {
            println!("{}", e);
            return;
        }
    };

    let statements = match Parser::new(tokens).parse() {
        Ok(vec) => {
            println!("{:#?}", vec);
            vec
        }
        Err(e) => {
            println!("{}", e);
            return;
        }
    };

    let mut codegen = CodeGen::new(statements);

    match codegen.assemble() {
        Ok(()) => (),
        Err(e) => {
            println!("{}", e);
        }
    };

    for i in codegen.out {
        print!("{}", i);
    }
}
