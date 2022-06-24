use std::fs;
use std::io::prelude::*;

pub struct FileGen {
    out_filename: String,
    data: Vec<u8>,
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
    pub fn new(out_filename: &str, data: Vec<u8>) -> Self {
        Self {
            out_filename: out_filename.to_owned(),
            data,
        }
    }

    pub fn generate_file(&self) -> Result<(), String> {
        // create or truncate file
        let mut f = match fs::File::create(&self.out_filename) {
            Ok(f) => f,
            Err(e) => return Err(format!("Error opening output file.\n Error:\n  {}", e)),
        };

        match f.set_len(0) {
            Ok(_) => (),
            Err(e) => return Err(format!("Error clearing output file.\n Error:\n  {}", e)),
        };

        match f.write_all(&self.data) {
            Ok(_) => (),
            Err(e) => return Err(format!("Error writing to output file.\n Error:\n  {}", e)),
        }

        Ok(())
    }
}
