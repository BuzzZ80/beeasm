use std::fmt;

#[derive(Debug, Clone)]
pub enum TokenKind {
    // Literals
    Integer(u16),
    Byte(u8),
    String(String),
    Label(String),

    // Symbols
    Arrow,
    QuestionMark,
    Colon,
    Plus,
    Minus,
    Asterisk,
    Slash,
    OpenParen,
    CloseParen,

    // Keywords
    // Registers
    G0,
    G1,
    G2,
    G3,
    G4,
    G5,
    Ix,
    Pc,
    // Instructions
    Add,
    Sub,
    Mul,
    Div,
    Mov,
    Inc,
    Dec,
    Cmp,
    Ldr,
    Str,
    Ldx,
    Stx,
    Lsl,
    Lsr,
    Ssp,
    Gsp,
    Or,
    And,
    Not,
    Xor,
    Flg,
    Push,
    Pshx,
    Pop,
    Adc,
    Sbc,
    Popx,
    Jmp,
    Jsr,
    Rts,
    Int,
    Cli,
    Sti,
    Exit,
    // Conditionals
    Eq,
    Neq,
    Lt,
    Gte,
    Gt,
    Lte,
    Cr,
    Ncr,

    // Assembler directives
    Org,
    Db,
    Fill,
    FillTo,
    Strz,
    //Other
    None,
}

/// TokenKind, span, line number
pub struct Token(pub TokenKind, pub usize, pub usize);

#[derive(Debug)]
pub struct Lexer<'a> {
    data: &'a str,
    span: (usize, usize),
    line: usize,
}

/// Returns a portion of a data from the start until pred returns false
fn take_while<F>(data: &str, pred: F) -> Result<(&str, usize), String>
where
    F: Fn(char) -> bool,
{
    let mut index = 0;

    for c in data.chars() {
        if !pred(c) {
            break;
        }
        index += c.len_utf8();
    }

    if index == 0 {
        Err("No matches".to_owned())
    } else {
        Ok((&data[..index], index))
    }
}

/// Returns the length of a span of whitespace excluding newlines
fn skip_white_space(data: &str) -> usize {
    match take_while(data, |c| c.is_whitespace() && c != '\n') {
        Ok((_, bytes_read)) => bytes_read,
        Err(_) => 0,
    }
}

/// Returns the length of a span from a ; to a newline
fn skip_comment(data: &str) -> usize {
    if data.starts_with(';') {
        let bytes_read = match take_while(data, |c| c != '\n') {
            Ok((_, bytes_read)) => bytes_read,
            Err(_) => panic!("Unexpected EOF in skip_comment"),
        };
        return bytes_read;
    }

    0
}

/// Returns the integer value of any decimal, binary, or hex string that data starts with
fn tokenize_number(data: &str) -> Result<Token, String> {
    let (read, bytes_read) = take_while(data, |c| !c.is_whitespace())?;

    let result_num = if read.len() > 2 {
        match &read[0..1] {
            "$" => u16::from_str_radix(&read[1..], 16),
            "%" => u16::from_str_radix(&read[1..], 2),
            _ => match &read[0..2] {
                "0x" => u16::from_str_radix(&read[2..], 16),
                "0b" => u16::from_str_radix(&read[2..], 2),
                _ => read.parse::<u16>(),
            },
        }
    } else {
        read.parse::<u16>()
    };

    let num = match result_num {
        Ok(n) => n,
        Err(_) => return Err(format!("Could not parse number: '{}'", read)),
    };

    Ok(Token(TokenKind::Integer(num), bytes_read, 0))
}

fn tokenize_byte(data: &str) -> Result<Token, String> {
    let (read, bytes_read) = take_while(data, |c| !c.is_whitespace())?;

    let result_num = if read.len() > 3 {
        match &read[1..2] {
            "$" => u8::from_str_radix(&read[2..], 16),
            "%" => u8::from_str_radix(&read[2..], 2),
            _ => match &read[1..3] {
                "0x" => u8::from_str_radix(&read[3..], 16),
                "0b" => u8::from_str_radix(&read[3..], 2),
                _ => read[1..].parse::<u8>(),
            },
        }
    } else {
        read[1..].parse::<u8>()
    };

    let num = match result_num {
        Ok(n) => n,
        Err(_) => return Err(format!("Could not parse byte: '{}'", read)),
    };

    Ok(Token(TokenKind::Byte(num), bytes_read, 0))
}

/// Returns a String from the 2nd char of data to the next ", will break if there's no "
fn tokenize_string_literal(data: &str) -> Result<Token, String> {
    let (read, bytes_read) = take_while(&data[1..], |c| c != '"')?;

    Ok(Token(TokenKind::String(read.to_owned()), bytes_read + 2, 0))
}

/// Returns a keyword or label from the start of data
fn tokenize_identifier(data: &str) -> Result<Token, String> {
    let (read, bytes_read) = take_while(data, |c| c == '_' || c.is_alphanumeric())?;

    let token_kind = match &read.to_lowercase()[..] {
        "g0" => TokenKind::G0,
        "g1" => TokenKind::G1,
        "g2" => TokenKind::G2,
        "g3" => TokenKind::G3,
        "g4" => TokenKind::G4,
        "g5" => TokenKind::G5,
        "ix" => TokenKind::Ix,
        "pc" => TokenKind::Pc,
        "add" => TokenKind::Add,
        "sub" => TokenKind::Sub,
        "mul" => TokenKind::Mul,
        "div" => TokenKind::Div,
        "mov" => TokenKind::Mov,
        "inc" => TokenKind::Inc,
        "dec" => TokenKind::Dec,
        "cmp" => TokenKind::Cmp,
        "ldr" => TokenKind::Ldr,
        "str" => TokenKind::Str,
        "ldx" => TokenKind::Ldx,
        "stx" => TokenKind::Stx,
        "lsl" => TokenKind::Lsl,
        "lsr" => TokenKind::Lsr,
        "ssp" => TokenKind::Ssp,
        "gsp" => TokenKind::Gsp,
        "or" => TokenKind::Or,
        "and" => TokenKind::And,
        "not" => TokenKind::Not,
        "xor" => TokenKind::Xor,
        "flg" => TokenKind::Flg,
        "push" => TokenKind::Push,
        "pshx" => TokenKind::Pshx,
        "pop" => TokenKind::Pop,
        "adc" => TokenKind::Adc,
        "sbc" => TokenKind::Sbc,
        "popx" => TokenKind::Popx,
        "jmp" => TokenKind::Jmp,
        "jsr" => TokenKind::Jsr,
        "rts" => TokenKind::Rts,
        "int" => TokenKind::Int,
        "cli" => TokenKind::Cli,
        "sti" => TokenKind::Sti,
        "exit" => TokenKind::Exit,
        "eq" | "zr" => TokenKind::Eq,
        "neq" | "nzr" => TokenKind::Neq,
        "lt" => TokenKind::Lt,
        "lte" => TokenKind::Lte,
        "gt" => TokenKind::Gt,
        "gte" => TokenKind::Gte,
        "cr" | "br" => TokenKind::Cr,
        "ncr" | "nbr" => TokenKind::Ncr,
        s => TokenKind::Label(s.to_owned()),
    };

    Ok(Token(token_kind, bytes_read, 0))
}

/// Tokenizes a single directive
fn tokenize_directive(data: &str) -> Result<Token, String> {
    let (read, bytes_read) = take_while(data, |c| c == '_' || c == '.' || c.is_alphanumeric())?;

    let token_kind = match &read.to_lowercase()[..] {
        ".org" => TokenKind::Org,
        ".db" => TokenKind::Db,
        ".fill" => TokenKind::Fill,
        ".fillto" => TokenKind::FillTo,
        ".strz" => TokenKind::Strz,
        s => return Err(format!("Unknown dot directive '{}'.", s)),
    };

    Ok(Token(token_kind, bytes_read, 0))
}

/// Tokenizes any character, string, integer, keyword, label, etc. Does not skip comments or whitespace
pub fn tokenize_one_token(data: &str) -> Result<Token, String> {
    let mut chars = data.chars();
    let next = match chars.next() {
        Some(c) => c,
        None => return Err("Unexpected EOF".to_owned()),
    };
    let peek = chars.next().unwrap_or('\0');

    let token = match next {
        '-' if peek == '>' => Token(TokenKind::Arrow, 2, 0),
        '>' => Token(TokenKind::Arrow, 1, 0),
        '?' => Token(TokenKind::QuestionMark, 1, 0),
        ':' => Token(TokenKind::Colon, 1, 0),
        '+' => Token(TokenKind::Plus, 1, 0),
        '-' => Token(TokenKind::Minus, 1, 0),
        '*' => Token(TokenKind::Asterisk, 1, 0),
        '/' => Token(TokenKind::Slash, 1, 0),
        '(' => Token(TokenKind::OpenParen, 1, 0),
        ')' => Token(TokenKind::CloseParen, 1, 0),
        '.' => tokenize_directive(data)?,
        '#' => tokenize_byte(data)?,
        '0'..='9' | '$' | '%' => tokenize_number(data)?,
        '"' => tokenize_string_literal(data)?,
        c @ '_' | c if c.is_alphanumeric() => tokenize_identifier(data)?,
        c => return Err(format!("Unexpected character {}", c)),
    };

    Ok(token)
}

impl Token {
    /// Converts Token to (TokenKind, usize)
    pub fn as_tuple(&self) -> (TokenKind, usize) {
        (self.0.to_owned(), self.1)
    }
}

impl<'a> Lexer<'a> {
    /// Creates a new, valid Lexer struct from a &str
    pub fn new(data: &'a str) -> Self {
        Self {
            data,
            span: (0, data.len()),
            line: 1,
        }
    }

    /// Tokenizes all of self.data, returning a Vec of all the tokens to be passed to a parser
    pub fn tokenize(&mut self) -> Result<Vec<Token>, String> {
        let mut tokens = Vec::new();

        while self.span.0 != self.span.1 {
            let (val, consumed) = match self.data.chars().nth(self.span.0).unwrap_or_else(|| {
                panic!(
                    "Lexer object span broke.\n{:#?}\nDid you forget a '\"'?\n",
                    self
                )
            }) {
                c if c.is_whitespace() && c != '\n' => {
                    (TokenKind::None, skip_white_space(self.get_selected()))
                }
                ';' => (TokenKind::None, skip_comment(self.get_selected())),
                '\n' => {
                    self.line += 1;
                    (TokenKind::None, 1)
                }
                _ => match tokenize_one_token(self.get_selected()) {
                    Ok(tok) => tok.as_tuple(),
                    Err(e) => return Err(format!("Error on line {}:\n  {}", self.line, e)),
                },
            };

            self.consume(consumed);

            match val {
                TokenKind::None => {}
                _ => {
                    tokens.push(Token(val, consumed, self.line));
                }
            }
        }

        Ok(tokens)
    }

    /// Removes amount characters from the beginning of self.data by increasing self.span.0
    fn consume(&mut self, amount: usize) {
        let (start, end) = self.span;
        self.span = (start + amount, end);
    }

    /// Returns the portion of self.data selected by self.span
    fn get_selected(&self) -> &str {
        &self.data[self.span.0..self.span.1]
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(len {:0>3}) {:?}", self.1, self.0)
    }
}
