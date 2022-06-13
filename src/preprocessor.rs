pub struct Preprocessor {
    main: String,
    out: String,
    tmp_filename: String,
}

enum ReadingStatus {
    PassThrough,
    IgnoreDirectives,
    GetDirective,
}

impl Preprocessor {
    pub fn new(main: String) -> Self {
        Self {
            main,
            out: String::new(),
            tmp_filename: String::new(),
        }
    }

    pub fn process(&mut self) -> String {
        let mut status = ReadingStatus::PassThrough;
        for c in self.main.chars() {
            match c {
                '"' if matches!(status, ReadingStatus::PassThrough) => {
                    self.out.push(c);
                    status = ReadingStatus::IgnoreDirectives;
                }
                '"' if matches!(status, ReadingStatus::IgnoreDirectives) => {
                    self.out.push(c);
                    status = ReadingStatus::PassThrough;
                }
                _ if matches!(status, ReadingStatus::IgnoreDirectives) => {
                    self.out.push(c);
                }
                ' ' if matches!(status, ReadingStatus::GetDirective) => {
                    status = ReadingStatus::PassThrough;
                    self.tmp_filename.clear();
                }
                c if matches!(status, ReadingStatus::GetDirective) => {
                    self.tmp_filename.push(c);
                }
                '#' => {
                    status = ReadingStatus::GetDirective;
                }
                c => self.out.push(c),
            }
        }

        self.out.to_owned()
    }
}