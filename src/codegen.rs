use super::lexer::TokenKind;
use super::parser::{Expr, ExprKind};

pub struct CodeGen {
    pub out: Vec<i16>,
    labels: Vec<(String, usize)>,
    exprs: Vec<Expr>,
    index: usize,
    address: usize,
}

impl CodeGen {
    pub fn new(exprs: Vec<Expr>) -> Self {
        Self {
            out: vec![],
            labels: vec![],
            exprs,
            index: 0,
            address: 0,
        }
    }

    pub fn assemble_single_expr(&mut self) -> Result<(), String> {
        match self.instruction() {
            Ok(Some(())) => return Ok(()),
            Ok(None) => return Err("Not an instruction, others not implemented yet".to_owned()),
            Err(e) => return Err(e),
        }
    }

    fn instruction(&mut self) -> Result<Option<()>, String> {
        let (cond_kind, exprs) = match self.exprs.get(self.index) {
            Some(Expr {
                kind: ExprKind::Instruction(cond),
                exprs: exprs,
                line: _,
            }) => {
                self.index += 1;
                (*cond, *exprs)
            }
            _ => return Ok(None),
        };

        let cond_binary: i16 = match cond_kind {
            TokenKind::None => (0b0000 << 12),
            TokenKind::Neq => (0b0010 << 12),
            TokenKind::Eq => (0b0011 << 12),
            TokenKind::Gte => (0b0100 << 12),
            TokenKind::Lt => (0b0101 << 12),
            TokenKind::Lte => (0b0110 << 12),
            TokenKind::Gt => (0b01111 << 12),
            TokenKind::Cr => (0b1001 << 12),
            TokenKind::Ncr => (0b1001 << 12),
        };

        match self.op(exprs) {
            Ok(()) => Ok(Some(())),
            Err(e) => Err(e),
        }
    }

    fn op(&mut self, exprs: Vec<Expr>) -> Result<(), String> {
        if exprs.len() > 1 {
            return Err("Parsing error put multiple expressions within an Instruction... oops".to_owned());
        }

        let op_kind = match exprs.get(0) {
            Some(expr) => match expr.kind {
                ExprKind::Op(op_kind) => op_kind,
                _ => return Err("Parsing error failed to put an opcode in the instruction struct... oops".to_owned()),
            },
            None => return Err("Parsing error failed to put anything in the instruction struct... oops".to_owned()),
        };

        let mut params: Vec<(char, i16)> = vec![];
        for expr in exprs[0].exprs {
            match self.parameter(expr) {
                Ok(t) => params.push(t),
                Err(e) => return Err(e),
            };
        };

        let opcode = match exprs.len() {
            0 => match op_kind {
                TokenKind::Rts => 0x32,
                TokenKind::Exit => 0x3F,
                _ => return Err("Not enough parameters supplied".to_owned()),
            }
            1 => match params[0].0 {
                'r' => match op_kind {
                    TokenKind::Inc => 0x0C,
                    TokenKind::Dec => 0x0D,
                    TokenKind::Ssp => 0x19,
                    TokenKind::Gsp => 0x1A,
                    TokenKind::Not => 0x1F,
                    TokenKind::Push => 0x24,
                    TokenKind::Pop => 0x26,
                    _ => return Err("This operation does not take a register argument".to_owned()),
                }
                'i' => match op_kind {
                    TokenKind::Ssp => 0x18,
                    TokenKind::Push => 0x23,
                    TokenKind::Pshx => 0x25,
                    TokenKind::Popx => 0x2B,
                    TokenKind::Jmp => 0x30,
                    TokenKind::Jsr => 0x31,
                    _ => return Err("This operation does not take an immediate argument".to_owned()),
                }
                _ => return Err("Codegen error, param is not int or reg".to_owned()),
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
                        _ => return Err("This operation does not take two register arguments".to_owned()),
                    }
                    'i' => match op_kind {
                        TokenKind::Cmp => 0x0E,
                        TokenKind::Str => 0x11,
                        TokenKind::Stx => 0x13,
                        _ => return Err("This operation does not take a register and then an immediate".to_owned()),
                    }
                }
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
                        _ => return Err("This operation does not take an immediate and then a register".to_owned()),
                    }
                    'i' => return Err("Instruction does not support these parameters".to_owned()),
                    _ => return Err("Codegen error, param is not int or reg".to_owned()),
                }
                _ => return Err("Codegen error, param is not int or reg".to_owned()),
            }
        };

        Ok(())
    }

    fn parameter(&self, expr: Expr) -> Result<(char, i16), String> {
        match expr.kind {
            ExprKind::Register(r) => Ok(match r {
                TokenKind::G0 => ('r', 0),
                TokenKind::G1 => ('r', 1),
                TokenKind::G2 => ('r', 2),
                TokenKind::G3 => ('r', 3),
                TokenKind::G4 => ('r', 4),
                TokenKind::G5 => ('r', 5),
                TokenKind::Ix => ('r', 6),
                TokenKind::Pc => ('r', 7),
            }),
            ExprKind::Integer(n) => Ok(('i', n)),
            ExprKind::Label(n) => Err("NOT YET IMPLEMENTED: LABELS".to_owned()),
        }
    }
}
