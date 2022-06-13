use super::fileio::*;

pub struct Preprocessor {
    main: String,
}

impl Preprocessor {
    pub fn new(main: String) -> Self {
        Self {
            main,
        }
    }
}