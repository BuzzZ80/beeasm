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
    let in_filename = "test.beeasm";
    let out_filename = "test.bin";

    let program = fileio::read_to_string(in_filename)?;

    let tokens = Lexer::new(&program).tokenize()?;

    let statements = Parser::new(tokens).parse()?;

    let mut codegen = CodeGen::new(statements);

    codegen.assemble()?;

    for i in &codegen.out {
        print!("0x{:0>4X}\n", i);
    }

    let filegen = fileio::FileGen::new(out_filename, codegen.out);
    filegen.generate_file()?;

    Ok(())
}

// For using ? to print errors
impl fmt::Debug for ErrString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\n\n{}\n\n", self.0)
    }
}

impl From<String> for ErrString {
    fn from(value: String) -> Self {
        Self(value)
    }
}
