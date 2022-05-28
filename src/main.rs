use std::fs;

mod lexer;
mod parser;
use lexer::*;
use parser::*;

fn main() {
    let filename = "test.beeasm";
    let my_program = fs::read_to_string(filename).unwrap();

    match Lexer::new(&my_program).tokenize() {
        Ok(vec) => match Parser::new(vec).parse_one_statement() {
            Ok(Some(t)) => println!("{:#?}", t),
            Ok(None) => println!("Invalid code at start of file"),
            Err(e) => println!("{}", e),
        },
        Err(e) => {
            println!("{}", e);
            panic!("\n\n{}\n\n", e);
        }
    };
}
