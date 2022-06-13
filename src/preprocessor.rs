pub struct Preprocessor {
    main: String,
    out: String,
    current_directive: String,
    current_filename: String,
}

enum ReadingStatus {
    PassThrough,
    IgnoreDirectives(char),
    GetDirective,
    GetFilename,
}

impl Preprocessor {
    pub fn new(main: String) -> Self {
        Self {
            main,
            out: String::new(),
            current_directive: String::new(),
            current_filename: String::new(),
        }
    }

    // super::fileio::read_to_string(self.current_filename.as_str())?.as_str(),

    pub fn process(&mut self) -> Result<String, String> {
        let mut status = ReadingStatus::PassThrough;
        for c in self.main.chars() {
            match status {
                // Ignore directives within string literals or comments
                ReadingStatus::PassThrough => match c {
                    '"' => {
                        self.out.push(c);
                        status = ReadingStatus::IgnoreDirectives('"');
                    }
                    ';' => {
                        self.out.push(c);
                        status = ReadingStatus::IgnoreDirectives('\n');
                    },
                    '#' => {
                        status = ReadingStatus::GetDirective;
                    }
                    _ => self.out.push(c),
                }
                // Exit ignoring status once the char in the tuple is reached
                ReadingStatus::IgnoreDirectives(until) => {
                    self.out.push(c);
                    if until == c {
                        status = ReadingStatus::PassThrough;
                    }
                }
                ReadingStatus::GetDirective => {
                    match c {
                        ' ' => {
                            match self.current_directive.to_lowercase().as_str() {
                                "include" => status = ReadingStatus::GetFilename,
                                _ => return Err(format!("Unknown preprocessor directive {}", self.current_directive)),
                            }
                            self.current_directive.clear();
                        }
                        _ => self.current_directive.push(c),
                    }
                }
                ReadingStatus::GetFilename => {
                    match c {
                        '>' => {
                            if !matches!(&self.current_filename[..1], "<") {
                                return Err("Incorrect preprocessor format".to_owned());
                            }
                            match self.current_directive.as_str() {
                                "include" => {
                                    self.out.push('\n')
                                }
                                _ => return Err(format!("Unknown preprocessor directive {}", self.current_directive)),
                            }
                            self.current_directive.clear();
                            self.current_filename.clear();
                        }
                        _ => self.current_filename.push(c)
                    }
                }
            }
        }

        Ok(self.out.to_owned())
    }
}
