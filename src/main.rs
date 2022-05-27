use std::fs;

mod lexer;
use lexer::*;

fn main() {
    let filename = "test.beeasm";
    let my_program = fs::read_to_string(filename).unwrap();

    match Lexer::new(&my_program).tokenize() {
        Ok(vec) => {println!("{:#?}", vec); vec},
        Err(e)  => {println!("{}", e); panic!("\n\n{}\n\n", e);},
    };
}