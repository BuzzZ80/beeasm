use super::lexer::{Token, TokenKind};
use std::fmt;

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
    line: usize,
}

#[derive(Debug)]
pub enum ExprKind {
    Instruction(TokenKind), // Conditions only
    Op(TokenKind),          // Operations only
    Expression,
    Integer(i16),
    String(String),
    Label(String),
    Register(TokenKind), // Registers only
    // Unary,
    // Binary,
    // Grouping,
    Directive(TokenKind),
}

pub struct Expr {
    pub kind: ExprKind,
    pub exprs: Vec<Expr>,
    pub line: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            index: 0,
            line: 1,
        }
    }

    fn peek(&self) -> Option<&Token> {
        if self.index != self.tokens.len() {
            self.tokens.get(self.index)
        } else {
            None
        }
    }

    fn next(&mut self) -> Option<&Token> {
        match self.tokens.get(self.index) {
            Some(t) if self.index != self.tokens.len() => {
                self.index += 1;
                self.line = t.2;
                Some(t)
            }
            _ => None,
        }
    }

    /*
     *[X] statement   = instruction | directive | label
     *
     *[X] instruction = op | op "?" CONDITION
     *[X] op          = OPCODE | OPCODE (REGISTER | expression) | OPCODE (REGISTER | expression) "->" (REGISTER | expression)
     *[/] expression  = INTEGER | LABEL | unary | binary | grouping
     *[ ] unary       = OPERATOR_UNARY expression
     *[ ] binary      = expression OPERATOR_BINARY expression
     *[ ] grouping    = "(" expression ")"
     *
     *[X] directive   = DIRECTIVE (expression | STRING)*
     *
     *[X] label       = LABEL ":"
     */

    pub fn parse(&mut self) -> Result<Vec<Expr>, String> {
        let mut output: Vec<Expr> = Vec::new();

        loop {
            match self.parse_one_statement() {
                Ok(Some(statement)) => output.push(statement),
                Ok(None) => break,
                Err(e) => return Err(format!("Error on line {}:\n  {}", self.line, e)),
            };
        }

        Ok(output)
    }
    pub fn parse_one_statement(&mut self) -> Result<Option<Expr>, String> {
        match self.instruction() {
            Ok(Some(i)) => return Ok(Some(i)),
            Ok(None) => (),
            Err(e) => return Err(e),
        };

        match self.directive() {
            Ok(Some(d)) => return Ok(Some(d)),
            Ok(None) => (),
            Err(e) => return Err(e),
        };

        match self.label() {
            Ok(Some(d)) => return Ok(Some(d)),
            Ok(None) => (),
            Err(e) => return Err(e),
        };

        match self.peek() {
            Some(t) => Err(format!("Unexpected token '{}'", t)),
            None => Ok(None),
        }
    }

    fn instruction(&mut self) -> Result<Option<Expr>, String> {
        // Ensure that there's an operation to be read in
        let op = match self.op() {
            Ok(Some(op)) => op,
            Ok(None) => return Ok(None),
            Err(e) => return Err(e),
        };

        // place operation into new instruction struct
        let mut instruction = Expr {
            kind: ExprKind::Instruction(TokenKind::None),
            exprs: vec![],
            line: op.line,
        };
        instruction.exprs.push(op);

        // Peek for next token
        let peek = match self.peek() {
            Some(t) => t,
            None => return Ok(Some(instruction)),
        };

        // Check if there's a '?' for the conditional
        if !matches!(peek.0, TokenKind::QuestionMark) {
            return Ok(Some(instruction)); // If there's none, just return the op with no condition as an instruction
        }

        let current_line = peek.2;

        self.next(); // Consume the '?'

        // Get the next word for the condition if it exists
        let peek = match self.peek() {
            Some(t) => t,
            None => return Err(format!("No condition after '?' on line {}", current_line)),
        };

        // Check that the peeked token is in fact a condition, and if so, set that to op's cond
        match peek.0 {
            TokenKind::Eq
            | TokenKind::Neq
            | TokenKind::Lt
            | TokenKind::Gte
            | TokenKind::Gt
            | TokenKind::Lte
            | TokenKind::Cr
            | TokenKind::Ncr => instruction.kind = ExprKind::Instruction(peek.0.to_owned()),
            _ => return Err("No condition after '?'".to_owned()),
        };

        // Consume conditional token
        self.next();

        Ok(Some(instruction))
    }

    fn op(&mut self) -> Result<Option<Expr>, String> {
        let op_token = match self.peek() {
            Some(t) => t,
            None => return Ok(None),
        };

        let mut op = match op_token.0 {
            TokenKind::Add
            | TokenKind::Sub
            | TokenKind::Mul
            | TokenKind::Div
            | TokenKind::Mov
            | TokenKind::Inc
            | TokenKind::Dec
            | TokenKind::Cmp
            | TokenKind::Ldr
            | TokenKind::Str
            | TokenKind::Ldx
            | TokenKind::Stx
            | TokenKind::Lsl
            | TokenKind::Lsr
            | TokenKind::Ssp
            | TokenKind::Gsp
            | TokenKind::Or
            | TokenKind::And
            | TokenKind::Not
            | TokenKind::Xor
            | TokenKind::Flg
            | TokenKind::Push
            | TokenKind::Pop
            | TokenKind::Adc
            | TokenKind::Sbc
            | TokenKind::Pshx
            | TokenKind::Jmp
            | TokenKind::Jsr
            | TokenKind::Rts
            | TokenKind::Int
            | TokenKind::Cli
            | TokenKind::Sti
            | TokenKind::Exit => Expr {
                kind: ExprKind::Op(op_token.0.to_owned()),
                exprs: vec![],
                line: op_token.2,
            },
            _ => return Ok(None),
        };

        self.next();

        let param_res = self.register_or_expression();
        match param_res {
            Ok(Some(val)) => op.exprs.push(val),
            Ok(None) => return Ok(Some(op)),
            Err(e) => return Err(e),
        }

        // Check for an arrow
        match self.peek() {
            Some(Token(TokenKind::Arrow, _, _)) => (),
            _ => return Ok(Some(op)),
        };

        self.next();

        let param_res = self.register_or_expression();
        match param_res {
            Ok(Some(val)) => op.exprs.push(val),
            _ => return Err("No 2nd parameter after '->'".to_owned()),
        }

        Ok(Some(op))
    }

    fn register_or_expression(&mut self) -> Result<Option<Expr>, String> {
        let (value_token_kind, line) = match self.peek() {
            Some(t) => (t.0.to_owned(), t.2),
            None => return Ok(None),
        };

        // Check if there's a register token
        match value_token_kind {
            TokenKind::G0
            | TokenKind::G1
            | TokenKind::G2
            | TokenKind::G3
            | TokenKind::G4
            | TokenKind::G5
            | TokenKind::Ix
            | TokenKind::Pc => {
                self.next();
                return Ok(Some(Expr {
                    kind: ExprKind::Register(value_token_kind),
                    exprs: vec![],
                    line,
                }));
            }
            _ => (),
        };

        let expr = self.expression();
        match expr {
            Ok(Some(expr)) => Ok(Some(expr)),
            Ok(None) => Ok(None),
            Err(e) => Err(e),
        }
    }

    fn expression(&mut self) -> Result<Option<Expr>, String> {
        let mut expr = Expr {
            kind: ExprKind::Expression,
            exprs: vec![],
            line: 0,
        };

        let expr_token = match self.peek() {
            Some(t) => {
                expr.line = t.2;
                t
            }
            None => return Ok(None),
        };

        match self.tokens.get(self.index + 1) {
            Some(x) if matches!(x.0, TokenKind::Colon) => return Ok(None),
            _ => (),
        };

        match &expr_token.0 {
            TokenKind::Integer(v) => expr.exprs.push(Expr {
                kind: ExprKind::Integer(*v),
                exprs: vec![],
                line: expr_token.2,
            }),
            TokenKind::Label(n) => expr.exprs.push(Expr {
                kind: ExprKind::Label(n.to_owned()),
                exprs: vec![],
                line: expr_token.2,
            }),
            _ => return Ok(None),
        };

        self.next();

        Ok(Some(expr))
    }

    /*
    fn unary(&mut self) -> Result<Option<Expr>, String> {
        Err("UNIMPLEMENTED/TODO".to_owned())
    }

    fn binary(&mut self) -> Result<Option<Expr>, String> {
        Err("UNIMPLEMENTED/TODO".to_owned())
    }

    fn grouping(&mut self) -> Result<Option<Expr>, String> {
        Err("UNIMPLEMENTED/TODO".to_owned())
    }
    */

    fn directive(&mut self) -> Result<Option<Expr>, String> {
        let directive_token = match self.peek() {
            Some(t) => t,
            None => return Ok(None),
        };

        let kind = match &directive_token.0 {
            TokenKind::Org | TokenKind::Db | TokenKind::Fill | TokenKind::Strz => {
                &directive_token.0
            }
            _ => return Ok(None),
        };

        let mut directive = Expr {
            kind: ExprKind::Directive(kind.to_owned()),
            exprs: vec![],
            line: directive_token.2,
        };

        self.next();

        loop {
            match self.expression() {
                Ok(Some(expr)) => {
                    directive.exprs.push(expr);
                    continue;
                }
                Ok(None) => (),
                Err(e) => return Err(e),
            };
            match self.peek() {
                Some(Token(TokenKind::String(s), _, line)) => {
                    directive.exprs.push(Expr {
                        kind: ExprKind::String(s.to_owned()),
                        exprs: vec![],
                        line: *line,
                    });
                    self.next();
                }
                _ => break,
            };
        }

        Ok(Some(directive))
    }

    fn label(&mut self) -> Result<Option<Expr>, String> {
        let (label_token_kind, line) = match self.peek() {
            Some(t) => (&t.0, t.2),
            None => return Ok(None),
        };

        let kind = match label_token_kind {
            TokenKind::Label(n) => ExprKind::Label(n.to_owned()),
            _ => return Ok(None),
        };

        self.next();

        let colon_token = match self.peek() {
            Some(t) => t.to_owned(),
            None => return Ok(None),
        };

        match &colon_token.0 {
            TokenKind::Colon => (),
            _ => return Err("No ':' after label".to_owned()),
        };

        self.next();

        Ok(Some(Expr {
            kind,
            exprs: vec![],
            line,
        }))
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match write!(f, "{:?}", self.kind) {
            Ok(()) => (),
            Err(e) => return Err(e),
        }
        for expr in &self.exprs {
            match write!(f, " {}", expr) {
                Ok(()) => (),
                Err(e) => return Err(e),
            }
        }
        fmt::Result::Ok(())
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match write!(f, "{:?}", self.kind) {
            Ok(()) => (),
            Err(e) => return Err(e),
        }
        for expr in &self.exprs {
            match write!(f, " {}", expr) {
                Ok(()) => (),
                Err(e) => return Err(e),
            }
        }
        fmt::Result::Ok(())
    }
}
