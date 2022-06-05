use std::fs;

pub fn read_to_string(filename: &str) -> Result<String, String> {
    match fs::read_to_string(filename) {
        Ok(str) => Ok(str),
        Err(e) => Err(format!("Problem with file '{}'.\n  Error:\n    {}", filename, e)),
    }
}