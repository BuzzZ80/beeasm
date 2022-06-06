use std::io::prelude::*;
use std::fs;

pub struct FileGen {
    out_filename: String,
    packets: Vec<i16>,
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
    pub fn new(out_filename: &str, packets: Vec<i16>) -> Self {
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

        let mut output: Vec<u8> = b"uwu".to_vec();

        match f.write_all(&output[..]) {
            Ok(()) => (),
            Err(e) => return Err(format!("Couldn't write to file.\n Error:\n  {}", e)),
        }

        Ok(())
    }
}
