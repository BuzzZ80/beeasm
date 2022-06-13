use super::fileio::*;

pub struct Preprocessor<'a> {
    main: &'a str,
    out: String
}

impl<'a> Preprocessor<'a> {
    pub fn new(main: &'a str) -> Self {
        Self {
            main,
            out: String::new(),
        }
    }

    pub fn process(&mut self) -> String {
        

        self.out.to_owned()
    }
}