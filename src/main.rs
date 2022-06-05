use std::fmt;

mod codegen;
mod fileio;
mod lexer;
mod parser;

use codegen::*;
use fileio::*;
use lexer::*;
use parser::*;

struct ErrString(String); // for using ? to print errors

fn main() -> Result<(), ErrString> {
    let filename = "test.beeasm";
    let program = fileio::read_to_string(filename)?;

    let tokens = Lexer::new(&program).tokenize()?;

    let statements = Parser::new(tokens).parse()?;

    let mut codegen = CodeGen::new(statements);

    codegen.assemble()?;

    for i in codegen.out {
        print!("{}", i);
    }

    Ok(())
}

// For using ? to print errors
impl fmt::Debug for ErrString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\n{}\n", self.0)
    }
}

impl From<String> for ErrString {
    fn from(value: String) -> Self {
        Self(value)
    }
}
