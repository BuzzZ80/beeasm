use std::collections::HashMap;
use super::lexer::TokenKind;
use super::parser::{Expr, ExprKind};

pub struct CodeGen {
    pub out: Vec<i16>,
    labels: HashMap<String, usize>,
    exprs: Vec<Expr>,
    index: usize,
}

impl CodeGen {
    pub fn new(exprs: Vec<Expr>) -> Self {
        Self {
            out: vec![],
            labels: HashMap::new(),
            exprs,
            index: 0,
        }
    }

    pub fn get_labels(&mut self) -> Result<(), String> {
        let mut address = 0;
        let to_return = loop {
            let (label, pos) = match self.get_next_label() {
                Ok(Some( (label, pos) )) => {
                    address += pos;
                    (label, address)
                }
                Ok(None) => break Ok(()),
                Err(e) => break Err(e),
            };

            match self.labels.get(&label) {
                Some(_) => return Err(format!("Duplicate label \"{}\" found", label)),
                None => ()
            };

            self.labels.insert(label, pos);
        };
        self.index = 0;
        to_return
    }

    pub fn assemble_single_expr(&mut self) -> Result<(), String> {
        match self.instruction() {
            Ok(Some(())) => Ok(()),
            Ok(None) => return Err("Encountered EOF or unsupported".to_owned()),
            Err(e) => Err(e),
        }
    }

    fn get_next_label(&mut self) -> Result<Option<(String, usize)>, String> {
        let mut addr = 0;
        let label = loop {
            let kind = match self.exprs.get(self.index) {
                Some(expr) => &expr.kind, 
                None => return Ok(None),
            };


            match kind {
                ExprKind::Instruction(_) => match self.instruction_len() {
                    Ok(n) => addr += n,
                    Err(e) => return Err(e),
                }
                ExprKind::Label(s) => break s.to_owned(),
                ExprKind::Directive(_) => panic!("Directives not supported yet"),
                _ => panic!("Non-instruction/label/directive found in get_next_label... oops"),
            }

            self.index += 1;
        };

        self.exprs.remove(self.index);
        return Ok(Some( (label, addr) ))
    }

    fn instruction_len(&self) -> Result<usize, String> {
        let exprs = match self.exprs.get(self.index) {
            Some(Expr {
                kind: ExprKind::Instruction(_),
                exprs,
                line: _,
            }) => exprs,
            _ => panic!("instruction_len called on non-instruction value... oops"),
        };

        let data = match self.op(exprs) {
            Ok(n) if !n.is_empty() => n,
            Ok(n) => panic!("Codegen error, only {} returned by op()... oops", n.len()),
            Err(e) => return Err(e),
        };

        data[0];

        Ok(data.len())
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

        let cond_binary: i16 = match cond_kind {
            TokenKind::None => 0b0000,
            TokenKind::Neq => 0b0010,
            TokenKind::Eq => 0b0011,
            TokenKind::Gte => 0b0100,
            TokenKind::Lt => 0b0101,
            TokenKind::Lte => 0b0110,
            TokenKind::Gt => 0b01111,
            TokenKind::Cr => 0b1001,
            TokenKind::Ncr => 0b1001,
            _ => panic!("Parsing error put an invalid condition in an instruction... oops"),
        };

        let mut data = match self.op(exprs) {
            Ok(n) if !n.is_empty() => n,
            Ok(n) => panic!("Codegen error, only {} returned by op()... oops", n.len()),
            Err(e) => return Err(e),
        };

        data[0] |= cond_binary << 6;

        self.out.append(&mut data);

        Ok(Some(()))
    }

    fn op(&self, exprs: &[Expr]) -> Result<Vec<i16>, String> {
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

        let mut params: Vec<(char, i16)> = vec![];
        for expr in &exprs[0].exprs {
            match self.parameter(expr) {
                Ok(t) => params.push(t),
                Err(e) => return Err(e),
            };
        }

        let mut opcode = match params.len() {
            0 => match op_kind {
                TokenKind::Rts => 0x32,
                TokenKind::Exit => 0x3F,
                _ => return Err("Not enough parameters supplied".to_owned()),
            },
            1 => {
                match params[0].0 {
                    'r' => match op_kind {
                        TokenKind::Inc => 0x0C,
                        TokenKind::Dec => 0x0D,
                        TokenKind::Ssp => 0x19,
                        TokenKind::Gsp => 0x1A,
                        TokenKind::Not => 0x1F,
                        TokenKind::Push => 0x24,
                        TokenKind::Pop => 0x26,
                        _ => {
                            return Err("This operation does not take a single register argument"
                                .to_owned())
                        }
                    },
                    'i' => match op_kind {
                        TokenKind::Ssp => 0x18,
                        TokenKind::Push => 0x23,
                        TokenKind::Pshx => 0x25,
                        TokenKind::Popx => 0x2B,
                        TokenKind::Jmp => 0x30,
                        TokenKind::Jsr => 0x31,
                        _ => {
                            return Err("This operation does not take a single immediate argument"
                                .to_owned())
                        }
                    },
                    _ => panic!("Codegen error, param is not int or reg... oops"),
                }
            }
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
                        _ => {
                            return Err(
                                "This operation does not take two register arguments".to_owned()
                            )
                        }
                    },
                    'i' => match op_kind {
                        TokenKind::Cmp => 0x0E,
                        TokenKind::Str => 0x11,
                        TokenKind::Stx => 0x13,
                        _ => {
                            return Err(
                                "This operation does not take a register and then an immediate"
                                    .to_owned(),
                            )
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
                            return Err(
                                "This operation does not take an immediate and then a register"
                                    .to_owned(),
                            )
                        }
                    },
                    'i' => return Err("Instruction does not support these parameters".to_owned()),
                    _ => panic!("Codegen error, param is not int or reg... oops"),
                },
                _ => panic!("Codegen error, param is not int or reg... oops"),
            },
            _ => {
                panic!("Parsing error, more than 2 parameters were found in one operation... oops")
            }
        };

        opcode <<= 10;
        opcode |= match params.len() {
            0 => 0,
            1 => match &params[0] {
                ('r', n) => *n,
                ('i', _) => 0,
                _ => panic!("Codegen error, param is not int or reg... oops"),
            },
            2 => {
                let p1 = match &params[0] {
                    ('r', n) => *n,
                    ('i', _) => 0,
                    _ => panic!("Codegen error, param is not int or reg... oops"),
                };
                p1 | match &params[1] {
                    ('r', n) => *n << 3,
                    ('i', _) => 0,
                    _ => panic!("Codegen error, param is not int or reg... oops"),
                }
            }
            _ => {
                panic!("Parsing error, more than 2 parameters were found in one operation... oops")
            }
        };

        let mut output: Vec<i16> = vec![opcode];
        for p in &params {
            if p.0 == 'i' {
                output.push(p.1);
            }
        }

        Ok(output)
    }

    fn parameter(&self, expr: &Expr) -> Result<(char, i16), String> {
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
            ExprKind::Expression => match self.expression(&expr) {
                Ok(n) => Ok(('i', n)),
                Err(e) => return Err(e),
            }
            _ => panic!("Parser error did not put in a register, immediate, or label... oops"),
        }
    }

    fn expression(&self, expr: &Expr) -> Result<i16, String> {
        let Expr {
            kind: expr_kind,
            exprs,
            line: _,
        } = expr;

        match expr_kind {
            ExprKind::Expression => (),
            _ => panic!("Codegen error, expression() was called on a non-expression value"),
        };

        match exprs.len() {
            1 => match &exprs[0].kind {
                ExprKind::Integer(n) => return Ok(*n),
                ExprKind::Label(s) => match self.labels.get(s) {
                    Some(n) => return Ok(*n as i16),
                    None => return Err(format!("Label \"{}\" not found", s)),
                },
                _ => {
                    panic!("Parser error did not put an immediate or label in expression()... oops")
                }
            },
            _ => panic!("Parser error put too many parameters to one expression in expression()"),
        }
    }
}
