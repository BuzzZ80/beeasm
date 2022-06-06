use std::fmt;
use std::env;

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
    let args: Vec<_> = env::args().collect();
    
    let in_filename: &str;
    let out_filename: &str;

    match args.len() {
        1 => {
            return Err("Expected input filename and optional output filename".to_owned().into());
        }
        2 => {
            println!("Assuming out.bin for output file");
            in_filename = &args[1];
            out_filename = "out.bin";
        }
        3 => {
            in_filename = &args[1];
            out_filename = &args[2];
        }
        _ => {
            return Err("Too many command line arguments provided. See README for correct usage".to_owned().into());
        }
    }

    let program = fileio::read_to_string(in_filename)?;

    let tokens = Lexer::new(&program).tokenize()?;

    let statements = Parser::new(tokens).parse()?;

    let mut codegen = CodeGen::new(statements);

    codegen.assemble()?;

    //for i in &codegen.out {
    //    println!("0x{:0>4X}", i);
    //}

    let filegen = FileGen::new(out_filename, codegen.out);
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
