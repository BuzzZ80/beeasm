use std::io::prelude::*;
use std::fs;

use super::codegen::WordPacket;

pub struct FileGen {
    out_filename: String,
    packets: Vec<WordPacket>,
}

pub fn read_to_string(filename: &str) -> Result<String, String> {
    match fs::read_to_string(filename) {
        Ok(str) => Ok(str),
        Err(e) => Err(format!(
            "Problem with file '{}'.\n  Error:\n    {}",
            filename, e
        )),
    }
}

impl FileGen {
    pub fn new(out_filename: &str, packets: Vec<WordPacket>) -> Self {
        Self {
            out_filename: out_filename.to_owned(),
            packets,
        }
    }

    pub fn generate_file(&self) -> Result<(), String> {
        // create or truncate file
        let mut f = match fs::File::create(&self.out_filename) {
            Ok(f) => f,
            Err(e) => return Err(format!("Error opening outputfile.\n Error:\n  {}", e)),
        };

        // check for problematic intersections
        for i in 0..self.packets.len() - 1 {
            for j in i + 1..self.packets.len() {
                if self.packets[i].intersects(&self.packets[j]) {
                    return Err(format!(
                        "Code from {:0>4X} intersects with code from {:0>4X}.",
                        self.packets[i].0, self.packets[j].0
                    ));
                }
            }
        }

        let mut output: Vec<u8> = vec![];




        match f.write_all(&output[..]) {
            Ok(()) => (),
            Err(e) => return Err(format!("Couldn't write to file.\n Error:\n  {}", e)),
        }

        Ok(())
    }
}
