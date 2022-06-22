use super::lexer::TokenKind;
use super::parser::{Expr, ExprKind};
use std::collections::HashMap;

const POS_SRC: usize = 0;
const POS_DEST: usize = 13;
const POS_OPCODE: usize = 3;
const POS_CONDITION: usize = 9;

pub struct CodeGen {
    pub out: Vec<u8>,
    pub labels: HashMap<String, usize>,
    exprs: Vec<Expr>,
    index: usize,
    org: usize,
    org_change: bool,
    binary_pos: usize,
}

impl CodeGen {
    pub fn new(exprs: Vec<Expr>) -> Self {
        Self {
            out: vec![],
            labels: HashMap::new(),
            exprs,
            index: 0,
            org: 0,
            org_change: false,
            binary_pos: 0,
        }
    }

    /// Fills self.out with WordPackets of data according to the assembly file
    pub fn assemble(&mut self) -> Result<(), String> {
        // Calculate labels before-hand
        match self.get_labels() {
            Ok(()) => (),
            Err(e) => {
                let line = match self.exprs.get(self.index) {
                    Some(n) => n.line as i32,
                    None => self.exprs[self.exprs.len() - 1].line as i32,
                };
                return Err(format!("Error on line {}:\n  {}", line, e));
            }
        };

        // Iterate through all instructions and directives now, returning any errors and quitting at the end of the file
        loop {
            match self.assemble_single_expr() {
                Ok(Some(())) => (),
                Ok(None) => break,
                Err(e) => {
                    let line = match self.exprs.get(self.index) {
                        Some(n) => n.line as i32,
                        None => self.exprs[self.exprs.len() - 1].line as i32,
                    };
                    return Err(format!("Error on line {}:\n  {}", line, e));
                }
            };
        }

        Ok(())
    }

    /// Calculates all labels in a CodeGen instance, then resets it to be used by assebmly instructions.
    /// Consumes all Label Exprs.
    fn get_labels(&mut self) -> Result<(), String> {
        let mut cum_pos = 0;
        // Loop through all labels:
        while let Some((label, pos)) = self.get_next_label()? {
            // Make sure label doesn't already exist elsewhere to prevent confusion or user error
            if self.labels.get(&label).is_some() {
                return Err(format!(r#"Duplicate label "{}" found"#, label));
            };

            if self.org_change {
                self.org_change = false;
                cum_pos = 0;
            }

            cum_pos += pos;

            // Inserts label from get_next_label() at the labels position from the base plus the base
            self.labels.insert(label, cum_pos + self.org);
        }

        // Reset the codegen struct so that it can be used by assembly instructions
        self.index = 0;
        self.org = 0;
        self.binary_pos = 0;
        self.out.clear();

        Ok(())
    }

    /// Calculates the distance from the base of the WordPacket to the first label, then consumes it.
    fn get_next_label(&mut self) -> Result<Option<(String, usize)>, String> {
        let mut relative_pos = 0; // Distance from base of packet
        let label; // Will store the name of the label

        // Loop through instructions and directives until a label is found
        loop {
            // Get the ExprKind of the next expr in the program (should only be instruction or directive)
            let kind = match self.exprs.get(self.index) {
                Some(expr) => &expr.kind,
                None => return Ok(None),
            };

            let len: usize = match kind {
                ExprKind::Instruction(_) => self.instruction_len()?,
                ExprKind::Directive(_) => self.directive_len()?,
                // If it's a label, break out of the loop after setting variable 'label' to the label name
                ExprKind::Label(l) => {
                    label = l.to_owned();
                    break;
                }
                _ => panic!("Codegen error - get_next_label() encountered a non-instruction, directive, or label value... oops"),
            };

            if self.org_change {
                relative_pos = 0;
            }

            relative_pos += len;
            self.binary_pos += len;

            // Update last packet (in case there was an org) and move on to next Expr in the program
            self.index += 1;
        }

        self.exprs.remove(self.index);

        Ok(Some((label, relative_pos)))
    }

    /// Get the number of bytes that a valid Instruction takes
    fn instruction_len(&self) -> Result<usize, String> {
        // Get the op from within the Instruction, if an Instruction exists
        let exprs = match self.exprs.get(self.index) {
            Some(Expr {
                kind: ExprKind::Instruction(_),
                exprs,
                line: _,
            }) => exprs,
            _ => panic!("instruction_len called on non-instruction value... oops"),
        };

        // Makes sure there's (only) one op, otherwise there was a parser error
        if exprs.len() != 1 {
            panic!("Parsing error put multiple or no Exprs within an Instruction... oops")
        }

        let mut bytes = 2; // Start with 2 bytes for intial opcode

        // Go through the parameters of the op and add a word for every one that takes up memory (just expressions)
        for expr in &exprs[0].exprs {
            match expr.kind {
                ExprKind::Register(_) => (),
                ExprKind::Expression => bytes += 2,
                _ => panic!(
                    "Parsing error placed a non-expression or register value in an Op... oops"
                ),
            }
        }

        // Return the number of words in the instruction
        Ok(bytes)
    }

    /// Get the number of bytes that a valid Directive takes in memory
    fn directive_len(&mut self) -> Result<usize, String> {
        let expr = match self.exprs.get(self.index) {
            Some(expr) if matches!(expr.kind, ExprKind::Directive(_)) => expr,
            _ => panic!("directive_len called on non-instruction value... oops"),
        };

        let kind = match &expr.kind {
            ExprKind::Directive(k) => k,
            _ => panic!("directive() called on a non-directive expr... oops"),
        };

        match kind {
            TokenKind::Org => {
                if expr.exprs.len() != 1 {
                    return Err("Wrong number of parameters given to org".to_owned());
                }

                self.org = self.expression(&expr.exprs[0])? as usize;
                self.org_change = true;

                Ok(0)
            }
            TokenKind::Db => {
                let mut len = 0;

                for expr in &expr.exprs {
                    match &expr.kind {
                        ExprKind::Expression => len += 2,
                        ExprKind::String(s) => len += s.len(),
                        ExprKind::Byte(_) => len += 1,
                        _ => return Err(format!("{} is not an expression, byte, or string", expr)),
                    }
                }

                Ok(len)
            }
            TokenKind::Fill => {
                if expr.exprs.len() != 1 && expr.exprs.len() != 2 {
                    return Err("Wrong number of parameters given to fill".to_owned());
                }

                let len = self.expression(&expr.exprs[0])? as usize;

                Ok(len)
            }
            TokenKind::FillTo => {
                if expr.exprs.len() != 1 && expr.exprs.len() != 2 {
                    return Err("Wrong number of parameters given to fillto".to_owned());
                }

                let until_addr = self.expression(&expr.exprs[0])? as usize;

                Ok(until_addr - self.binary_pos)
            }
            TokenKind::Strz => {
                let mut len = 0;

                for expr in &expr.exprs {
                    match &expr.kind {
                        ExprKind::Expression => len += 2,
                        ExprKind::String(s) => len += s.len(),
                        ExprKind::Byte(_) => len += 1,
                        _ => return Err(format!("{} is not an expression, byte, or string", expr)),
                    }
                }

                Ok(len + 1)
            }
            TokenKind::Def => {
                let exprs = &expr.exprs;
                if exprs.len() != 2 {
                    return Err(".def takes an identifier and a 16-bit integer".to_owned());
                };

                let label = match &exprs[0].kind {
                    ExprKind::Expression => self.get_label_name(&exprs[0])?,
                    _ => return Err(format!("Expected identifier, found {:?}", &expr.kind)),
                };

                let value = match &exprs[1].kind {
                    ExprKind::Expression => self.expression(&exprs[1])? as usize,
                    _ => return Err(format!("Expected 16-bit integer, found {}", &exprs[1])),
                };

                if self.labels.get(&label).is_some() {
                    return Err(format!(r#"Duplicate label/identifier "{}" found"#, label));
                };

                self.labels.insert(label, value);

                Ok(0)
            }
            _ => panic!("Parser error put non-directive in directive expr... oops"),
        }
    }

    /// Takes an Instruction or Directive and modifies self accordingly
    fn assemble_single_expr(&mut self) -> Result<Option<()>, String> {
        match self.exprs.get(self.index) {
            Some(expr) => match &expr.kind {
                ExprKind::Instruction(_) => self.instruction(),
                ExprKind::Directive(_) => self.directive(),
                k => panic!(
                    "assemble_single_expr was called on a(n) {:?}, which is not supported.",
                    k
                ),
            },
            None => Ok(None),
        }
    }

    fn instruction(&mut self) -> Result<Option<()>, String> {
        let (cond_kind, exprs) = match self.exprs.get(self.index) {
            Some(Expr {
                kind: ExprKind::Instruction(cond),
                exprs,
                line: _,
            }) => {
                self.index += 1;
                (cond, exprs)
            }
            _ => return Ok(None),
        };

        let cond_binary: u16 = match cond_kind {
            TokenKind::None => 0b0000,
            TokenKind::Neq => 0b0010,
            TokenKind::Eq => 0b0011,
            TokenKind::Gte => 0b0100,
            TokenKind::Lt => 0b0101,
            TokenKind::Lte => 0b0110,
            TokenKind::Gt => 0b0111,
            TokenKind::Cr => 0b1001,
            TokenKind::Ncr => 0b1000,
            TokenKind::In => 0b1011,
            TokenKind::Nin => 0b1010,
            TokenKind::Ir => 0b1101,
            TokenKind::Nir => 0b1100,
            _ => panic!("Parsing error put an invalid condition in an instruction... oops"),
        };

        let mut data = match self.op(exprs)? {
            n if !n.is_empty() => n,
            n => panic!("Codegen error, only {} returned by op()... oops", n.len()),
        };

        data[0] |= cond_binary << POS_CONDITION;

        // Append to out
        for i in data {
            self.out.extend_from_slice(&i.to_le_bytes());
        }

        Ok(Some(()))
    }

    fn op(&self, exprs: &[Expr]) -> Result<Vec<u16>, String> {
        if exprs.len() > 1 {
            panic!("Parsing error put multiple Exprs within an Instruction... oops")
        }

        let op_kind = match exprs.get(0) {
            Some(expr) => match &expr.kind {
                ExprKind::Op(op_kind) => op_kind,
                _ => panic!(
                    "Parsing error failed to put an opcode in the instruction struct... oops"
                ),
            },
            None => {
                panic!("Parsing error failed to put anything in the instruction struct... oops")
            }
        };

        let mut params: Vec<(char, u16)> = vec![];
        for expr in &exprs[0].exprs {
            params.push(self.parameter(expr)?);
        }

        let mut opcode = match params.len() {
            0 => match op_kind {
                TokenKind::Scp => 0x08,
                TokenKind::Ccp => 0x09,
                TokenKind::Scb => 0x2D,
                TokenKind::Ccb => 0x2E,
                TokenKind::Scr => 0x2E,
                TokenKind::Clcr => 0x2F,
                TokenKind::Rts => 0x32,
                TokenKind::Sti => 0x36,
                TokenKind::Cli => 0x37,
                TokenKind::Rti => 0x38,
                TokenKind::Exit => 0x3F,
                _ => {
                    return Err(format!(
                        "Not enough parameters supplied to instruction '{:?}'",
                        op_kind
                    ))
                }
            },
            1 => match params[0].0 {
                'r' => match op_kind {
                    TokenKind::Inc => 0x0C,
                    TokenKind::Dec => 0x0D,
                    TokenKind::Ssp => 0x19,
                    TokenKind::Gsp => 0x1A,
                    TokenKind::Not => 0x1F,
                    TokenKind::Push => 0x24,
                    TokenKind::Pop => 0x26,
                    TokenKind::Clc => 0x2C,
                    _ => {
                        return Err(format!(
                            "'{:?}' does not take a single register argument",
                            op_kind
                        ))
                    }
                },
                'i' => match op_kind {
                    TokenKind::Ssp => 0x18,
                    TokenKind::Push => 0x23,
                    TokenKind::Pshx => 0x25,
                    TokenKind::Popx => 0x2B,
                    TokenKind::Jmp => 0x30,
                    TokenKind::Jsr => 0x31,
                    TokenKind::Int => 0x35,
                    _ => {
                        return Err(format!(
                            "'{:?}' does not take a single immediate argument",
                            op_kind
                        ))
                    }
                },
                _ => panic!("Codegen error, param is not int or reg... oops"),
            },
            2 => match params[0].0 {
                'r' => match params[1].0 {
                    'r' => match op_kind {
                        TokenKind::Add => 0x01,
                        TokenKind::Sub => 0x03,
                        TokenKind::Mul => 0x05,
                        TokenKind::Div => 0x07,
                        TokenKind::Mov => 0x0B,
                        TokenKind::Cmp => 0x0F,
                        TokenKind::Lsl => 0x15,
                        TokenKind::Lsr => 0x17,
                        TokenKind::Or => 0x1C,
                        TokenKind::And => 0x1E,
                        TokenKind::Xor => 0x21,
                        TokenKind::Adc => 0x28,
                        TokenKind::Sbc => 0x2A,
                        TokenKind::Scmp => 0x34,
                        TokenKind::Swap => 0x39,
                        TokenKind::Ldr => 0x3A,
                        TokenKind::Str => 0x3B,
                        _ => {
                            return Err(format!(
                                "'{:?}' does not take two register arguments",
                                op_kind
                            ))
                        }
                    },
                    'i' => match op_kind {
                        TokenKind::Cmp => 0x0E,
                        TokenKind::Str => 0x11,
                        TokenKind::Stx => 0x13,
                        TokenKind::Scmp => 0x33,
                        _ => {
                            return Err(format!(
                                "'{:?}' does not take a register and then an immediate",
                                op_kind
                            ))
                        }
                    },
                    _ => panic!("Codegen error, value not a variable or immediate... oops"),
                },
                'i' => match params[1].0 {
                    'r' => match op_kind {
                        TokenKind::Add => 0x00,
                        TokenKind::Sub => 0x02,
                        TokenKind::Mul => 0x04,
                        TokenKind::Div => 0x06,
                        TokenKind::Mov => 0x0A,
                        TokenKind::Ldr => 0x10,
                        TokenKind::Ldx => 0x12,
                        TokenKind::Lsl => 0x14,
                        TokenKind::Lsr => 0x16,
                        TokenKind::Or => 0x1B,
                        TokenKind::And => 0x1D,
                        TokenKind::Xor => 0x20,
                        TokenKind::Flg => 0x22,
                        TokenKind::Adc => 0x27,
                        TokenKind::Sbc => 0x29,
                        _ => {
                            return Err(format!(
                                "{:?} does not take an immediate and then a register",
                                op_kind
                            ))
                        }
                    },
                    'i' => {
                        return Err(format!(
                            "Two ints provided to '{:?}', which isn't supported",
                            op_kind
                        ))
                    }
                    _ => panic!("Codegen error, param is not int or reg... oops"),
                },
                _ => panic!("Codegen error, param is not int or reg... oops"),
            },
            _ => {
                panic!("Parsing error, more than 2 parameters were found in one operation... oops")
            }
        };

        opcode <<= POS_OPCODE;
        opcode |= match params.len() {
            0 => 0,
            1 => match &params[0] {
                ('r', n) => match op_kind {
                    TokenKind::Push => *n << POS_SRC,
                    _ => *n << POS_DEST,
                },
                ('i', _) => 0,
                _ => panic!("Codegen error, param is not int or reg... oops"),
            },
            2 => {
                let p1 = match &params[0] {
                    ('r', n) => *n << POS_SRC,
                    ('i', _) => 0,
                    _ => panic!("Codegen error, param is not int or reg... oops"),
                };
                p1 | match &params[1] {
                    ('r', n) => *n << POS_DEST,
                    ('i', _) => 0,
                    _ => panic!("Codegen error, param is not int or reg... oops"),
                }
            }
            _ => {
                panic!("Parsing error, more than 2 parameters were found in one operation... oops")
            }
        };

        let mut output: Vec<u16> = vec![opcode];
        for p in &params {
            if p.0 == 'i' {
                output.push(p.1);
            }
        }

        Ok(output)
    }

    fn parameter(&self, expr: &Expr) -> Result<(char, u16), String> {
        match &expr.kind {
            ExprKind::Register(r) => Ok(match r {
                TokenKind::G0 => ('r', 0),
                TokenKind::G1 => ('r', 1),
                TokenKind::G2 => ('r', 2),
                TokenKind::G3 => ('r', 3),
                TokenKind::G4 => ('r', 4),
                TokenKind::G5 => ('r', 5),
                TokenKind::Ix => ('r', 6),
                TokenKind::Pc => ('r', 7),
                _ => panic!("Parser error did not put a valid register in Register struct... oops\n  put: {:?}", r),
            }),
            ExprKind::Expression => Ok(('i', self.expression(expr)?)),
            _ => panic!("Parser error did not put in a register, immediate, or label... oops"),
        }
    }

    fn expression(&self, expr: &Expr) -> Result<u16, String> {
        let Expr {
            kind: expr_kind,
            exprs,
            line: _,
        } = expr;

        match expr_kind {
            ExprKind::Expression => (),
            _ => panic!("Codegen error, expression() was called on a non-expression value"),
        };

        let val = self.term(&exprs[0])?;

        Ok(val)
    }

    fn term(&self, expr: &Expr) -> Result<u16, String> {
        let mut val: u16 = 0;

        let mut operation = TokenKind::Plus;

        for expr in &expr.exprs {
            match &expr.kind {
                ExprKind::Operator(tk) => operation = tk.to_owned(),
                ExprKind::Factor => match operation {
                    TokenKind::Plus => val = val.wrapping_add(self.factor(expr)?),
                    TokenKind::Minus => val = val.wrapping_sub(self.factor(expr)?),
                    TokenKind::LeftShift => val <<= self.factor(expr)?,
                    TokenKind::RightShift => val >>= self.factor(expr)?,
                    tk => panic!("{:?} found as operation in codegen.term... oops", tk),
                },
                ek => panic!("{:?} found in term during codegen... oops", ek),
            }
        }

        Ok(val)
    }

    fn factor(&self, expr: &Expr) -> Result<u16, String> {
        let mut val: u16 = 0;

        let mut operation = TokenKind::Plus;

        for expr in &expr.exprs {
            match &expr.kind {
                ExprKind::Operator(tk) => operation = tk.to_owned(),
                ExprKind::Unary => match operation {
                    TokenKind::Plus => val = val.wrapping_add(self.unary(expr)?), // For initial load
                    TokenKind::Asterisk => val = val.wrapping_mul(self.unary(expr)?),
                    TokenKind::Slash => val = val.wrapping_div(self.unary(expr)?),
                    tk => panic!("{:?} found as operation in codegen.factor... oops", tk),
                },
                ek => panic!("{:?} found in factor during codegen... oops", ek),
            }
        }

        Ok(val)
    }

    fn unary(&self, expr: &Expr) -> Result<u16, String> {
        let mut val: u16 = 0;

        let mut operation = TokenKind::Plus;

        for expr in &expr.exprs {
            match &expr.kind {
                ExprKind::Operator(tk) => operation = tk.to_owned(),
                ExprKind::Unary => match operation {
                    TokenKind::Plus => val = val.wrapping_add(self.unary(expr)?),
                    TokenKind::Minus => val = val.wrapping_sub(self.unary(expr)?),
                    TokenKind::Tilde => val = !self.unary(expr)?,
                    tk => panic!("{:?} found as operation in codegen.unary... oops", tk),
                },
                ExprKind::Primary => match operation {
                    TokenKind::Plus => val = val.wrapping_add(self.primary(expr)?),
                    TokenKind::Minus => val = val.wrapping_sub(self.primary(expr)?),
                    TokenKind::Tilde => val = !self.primary(expr)?,
                    tk => panic!("{:?} found as operation in codegen.unary... oops", tk),
                },
                ek => panic!("{:?} found in unary during codegen... oops", ek),
            }
        }

        Ok(val)
    }

    fn primary(&self, expr: &Expr) -> Result<u16, String> {
        let mut val = 0;

        for expr in &expr.exprs {
            match &expr.kind {
                ExprKind::Integer(n) => val = *n,
                ExprKind::Label(s) => match self.labels.get(s) {
                    Some(n) => val = *n as u16,
                    None => return Err(format!("Label \"{}\" not found", s)),
                },
                ExprKind::Expression => val = self.expression(expr)?,
                ek => panic!("{:?} found in unary during codegen... oops", ek),
            }
        }

        Ok(val)
    }

    fn get_label_name(&self, expr: &Expr) -> Result<String, String> {
        let mut current = expr;
        loop {
            match current.exprs.get(0) {
                Some(Expr {
                    kind: ExprKind::Label(l),
                    exprs: _,
                    line: _,
                }) => {
                    return Ok(l.to_owned());
                }
                Some(e) => {
                    current = match current.exprs.get(0) {
                        Some(e) => e,
                        None => return Err(format!("Expected label, found {:?}", e)),
                    }
                }
                None => return Err(format!("Expected label, found {:?}", current)),
            }
        }
    }

    fn directive(&mut self) -> Result<Option<()>, String> {
        let expr = match self.exprs.get(self.index) {
            Some(expr) if matches!(expr.kind, ExprKind::Directive(_)) => expr,
            _ => panic!("directive() called on non-instruction value... oops"),
        };

        self.index += 1;

        let kind = match &expr.kind {
            ExprKind::Directive(k) => k,
            _ => panic!("directive() called on a non-directive expr... oops"),
        };

        match kind {
            TokenKind::Org => {
                if expr.exprs.len() != 1 {
                    return Err("Wrong number of parameters given to org".to_owned());
                }

                self.org = self.expression(&expr.exprs[0])? as usize;

                return Ok(Some(()));
            }
            TokenKind::Db => {
                for expr in &expr.exprs {
                    match &expr.kind {
                        ExprKind::Expression => {
                            self.out
                                .extend_from_slice(&self.expression(expr)?.to_le_bytes());
                        }
                        ExprKind::String(s) => {
                            for c in s.chars() {
                                self.out.push(c as u8);
                            }
                        }
                        ExprKind::Byte(n) => {
                            self.out.extend_from_slice(&n.to_le_bytes());
                        }
                        _ => return Err(format!("{} is not an expression, byte, or string", expr)),
                    }
                }
            }
            TokenKind::Fill => {
                let len = self.expression(&expr.exprs[0])? as usize;
                let fill_value = match expr.exprs.len() {
                    1 => 0x00,
                    2 => match &expr.exprs[1].kind {
                        ExprKind::Expression => {
                            let fill_value = self.expression(&expr.exprs[1])?;

                            if fill_value > u8::MAX as u16 || fill_value < u8::MIN as u16 {
                                return Err(format!("{:0>4X} is not a byte value", fill_value));
                            }

                            fill_value as u8
                        }
                        ExprKind::Byte(b) => *b,
                        _ => {
                            return Err(format!(
                                ".fill only takes an expression and then a byte. Found Expr {:?}",
                                self.exprs[self.index].kind
                            ))
                        }
                    },
                    _ => return Err("Wrong number of parameters given to .fill".to_owned()),
                };

                for _ in 0..len {
                    self.out.push(fill_value);
                }
            }
            TokenKind::FillTo => {
                let until_addr = self.expression(&expr.exprs[0])? as usize;
                let fill_value = match expr.exprs.len() {
                    1 => 0x00,
                    2 => match &expr.exprs[1].kind {
                        ExprKind::Expression => {
                            let fill_value = self.expression(&expr.exprs[1])?;

                            if fill_value > u8::MAX as u16 || fill_value < u8::MIN as u16 {
                                return Err(format!("{:0>4X} is not a byte value", fill_value));
                            }

                            fill_value as u8
                        }
                        ExprKind::Byte(b) => *b,
                        _ => {
                            return Err(format!(
                                ".fillto only takes an expression and then a byte. Found Expr {:?}",
                                self.exprs[self.index].kind
                            ))
                        }
                    },
                    _ => return Err("Wrong number of parameters given to .fillto".to_owned()),
                };

                for _ in self.out.len()..until_addr {
                    self.out.push(fill_value);
                }
            }
            TokenKind::Strz => {
                for expr in &expr.exprs {
                    match &expr.kind {
                        ExprKind::Expression => {
                            self.out
                                .extend_from_slice(&self.expression(expr)?.to_le_bytes());
                        }
                        ExprKind::String(s) => {
                            for c in s.chars() {
                                self.out.push(c as u8);
                            }
                        }
                        ExprKind::Byte(n) => {
                            self.out.extend_from_slice(&n.to_le_bytes());
                        }
                        _ => return Err(format!("{} is not an expression, byte, or string", expr)),
                    }
                }
                self.out.push(0u8);
            }
            TokenKind::Def => {}
            _ => panic!("Parser error put non-directive in directive expr... oops"),
        };

        Ok(Some(()))
    }
}
