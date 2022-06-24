pub struct Preprocessor {
    main: String,
    out: String,
    current_directive: String,
    current_filename: String,
    line: usize,
}

#[derive(Debug)]
enum ReadingStatus {
    PassThrough,
    IgnoreDirectives(char),
    IgnoreNext(Box<ReadingStatus>),
    GetDirective,
    GetFilename(usize),
}

impl Preprocessor {
    pub fn new(main: String) -> Self {
        Self {
            main,
            out: String::new(),
            current_directive: String::new(),
            current_filename: String::new(),
            line: 1,
        }
    }

    pub fn process(&mut self) -> Result<String, String> {
        match self.process_no_error_line() {
            Ok(s) => Ok(s),
            Err(s) => Err(format!("Error on line {}:\n  {}", self.line, s)),
        }
    }

    fn process_no_error_line(&mut self) -> Result<String, String> {
        let mut status = ReadingStatus::PassThrough;
        for c in self.main.chars() {
            if c == '\n' {
                self.line += 1;
            }
            match status {
                s if c == '\\' => {
                    self.out.push(c);
                    status = ReadingStatus::IgnoreNext(Box::new(s));
                }
                ReadingStatus::IgnoreNext(s) => {
                    self.out.push(c);
                    status = *s;
                }
                // Ignore directives within string literals or comments
                ReadingStatus::PassThrough => match c {
                    '"' => {
                        self.out.push(c);
                        status = ReadingStatus::IgnoreDirectives('"');
                    }
                    '\'' => {
                        self.out.push(c);
                        status = ReadingStatus::IgnoreDirectives('\'');
                    }
                    ';' => {
                        self.out.push(c);
                        status = ReadingStatus::IgnoreDirectives('\n');
                    }
                    '#' => {
                        status = ReadingStatus::GetDirective;
                    }
                    _ => self.out.push(c),
                },
                // Exit ignoring status once the char in the tuple is reached
                ReadingStatus::IgnoreDirectives(until) => {
                    self.out.push(c);
                    if until == c {
                        status = ReadingStatus::PassThrough;
                    }
                }
                ReadingStatus::GetDirective => match c {
                    _ if c.is_whitespace() => {
                        match self.current_directive.to_lowercase().as_str() {
                            "include" => status = ReadingStatus::GetFilename(0),
                            _ => {
                                return Err(format!(
                                    "Unknown preprocessor directive \"{}\"",
                                    self.current_directive
                                ))
                            }
                        }
                    }
                    _ => self.current_directive.push(c),
                },
                ReadingStatus::GetFilename(0) => {
                    status = ReadingStatus::GetFilename(1);
                    match c {
                        '<' => {}
                        c if c.is_whitespace() => {}
                        c => {
                            return Err(format!(
                                "Expected opening bracket '<' for file name. Found '{}'",
                                c
                            ))
                        }
                    }
                }
                ReadingStatus::GetFilename(n) => {
                    match c {
                        '>' => {
                            match self.current_directive.as_str() {
                                "include" => {
                                    self.out.push('\n');
                                    let sub_program = super::fileio::read_to_string(
                                        self.current_filename.as_str(),
                                    )?;
                                    let mut sub_processor = Preprocessor::new(sub_program);
                                    self.out.push_str(sub_processor.process()?.as_str());
                                }
                                _ => {
                                    return Err(format!(
                                        "Unknown preprocessor directive \"{}\"",
                                        self.current_directive
                                    ))
                                }
                            }
                            self.current_directive.clear();
                            self.current_filename.clear();
                            status = ReadingStatus::PassThrough;
                            continue;
                        }
                        _ => self.current_filename.push(c),
                    };
                    status = ReadingStatus::GetFilename(n + 1);
                }
            }
        }

        Ok(self.out.to_owned())
    }
}
