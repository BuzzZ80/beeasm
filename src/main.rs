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
            Some(Ok(t)) => println!("{:#?}", t),
            Some(Err(e)) => println!("{}", e),
            None => panic!("OnO"),
        },
        Err(e) => {
            println!("{}", e);
            panic!("\n\n{}\n\n", e);
        }
    };
}
