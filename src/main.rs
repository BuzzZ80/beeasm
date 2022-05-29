use std::fs;

mod lexer;
mod parser;
use lexer::*;
use parser::*;

fn main() {
    let filename = "test.beeasm";
    let my_program = fs::read_to_string(filename).unwrap();

    let tokens = match Lexer::new(&my_program).tokenize() {
        Ok(vec) => {
            //println!("{:#?}", vec);
            vec
        }
        Err(e) => {
            println!("{}", e);
            panic!();
        }
    };

    let mut parser = Parser::new(tokens);

    loop {
        match parser.parse_one_statement() {
            Ok(Some(t)) => println!("{}", t),
            Ok(None) => {
                println!("EOF");
                break;
            }
            Err(e) => {
                println!("{}", e);
                panic!();
            }
        }
    }
}
