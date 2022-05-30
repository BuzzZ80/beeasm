use super::lexer::{Token, TokenKind};
use std::fmt;

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
    line: usize,
}

#[derive(Debug)]
#[allow(dead_code)]
enum ExprKind {
    Instruction(TokenKind), // Conditions only
    Op(TokenKind),          // Operations only
    Value,
    // Expression,// Removed because i dont think theres any benefit to wrapping expressions in this
    Literal(i16),
    Label(String),
    Register(TokenKind), // Registers only
    Unary,
    Binary,
    Grouping,
    Directive(TokenKind),
}

#[derive(Debug)]
pub struct Expr {
    kind: ExprKind,
    exprs: Vec<Expr>,
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
     * statement   = instruction | directive | label
     * instruction = op | op "?" CONDITION
     * op          = OPCODE | OPCODE value | OPCODE value "->" value
     * value       = REGISTER | expression
     * expression  = literal | LABEL | unary | binary | grouping
     * literal     = INTEGER
     * unary       = OPERATOR_UNARY expression
     * binary      = expression OPERATOR_BINARY expression
     * grouping    = "(" expression ")"
     *
     * directive   = DIRECTIVE (expression | STRING)*
     *
     * label       = LABEL ":"
     */

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
            Some(t) => return Err(format!("Unexpected token '{}' on line '{}'", t, self.line)),
            None => return Ok(None),
        }
    }

    fn instruction(&mut self) -> Result<Option<Expr>, String> {
        let mut instruction = Expr {
            kind: ExprKind::Instruction(TokenKind::None),
            exprs: vec![],
        };
        // Ensure that there's an operation to be read in
        let op = match self.op() {
            Ok(Some(op)) => op,
            Ok(None) => return Ok(None),
            Err(e) => return Err(e),
        };

        // place operation into instruction
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
            _ => return Err(format!("No condition after '?' on line {}", current_line)),
        };

        self.next();

        Ok(Some(instruction))
    }

    fn op(&mut self) -> Result<Option<Expr>, String> {
        let op_token_kind = match self.peek() {
            Some(t) => t.as_tuple().0,
            None => return Ok(None),
        };

        let mut op = match op_token_kind {
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
            | TokenKind::Asl
            | TokenKind::Asr
            | TokenKind::Ssp
            | TokenKind::Gsp
            | TokenKind::Or
            | TokenKind::And
            | TokenKind::Not
            | TokenKind::Xor
            | TokenKind::Flg
            | TokenKind::Push
            | TokenKind::Pshx
            | TokenKind::Pop
            | TokenKind::Adc
            | TokenKind::Sbc
            | TokenKind::Jmp
            | TokenKind::Jsr
            | TokenKind::Rts
            | TokenKind::Int
            | TokenKind::Cli
            | TokenKind::Sti
            | TokenKind::Exit => Expr {
                kind: ExprKind::Op(op_token_kind),
                exprs: vec![],
            },
            _ => return Ok(None),
        };

        self.next();

        let param_res = self.value();
        match param_res {
            Ok(Some(val)) => op.exprs.push(val),
            Ok(None) => return Ok(Some(op)),
            Err(e) => return Err(e),
        }

        // Check for an arrow
        let line = match self.peek() {
            Some(Token(TokenKind::Arrow, _, line)) => *line,
            _ => return Ok(Some(op)),
        };

        self.next();

        let param_res = self.value();
        match param_res {
            Ok(Some(val)) => op.exprs.push(val),
            _ => return Err(format!("No 2nd parameter after -> on line {}", line)),
        }

        Ok(Some(op))
    }

    fn value(&mut self) -> Result<Option<Expr>, String> {
        let mut value = Expr {
            kind: ExprKind::Value,
            exprs: vec![],
        };

        let value_token_kind = match self.peek() {
            Some(t) => t.as_tuple().0,
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
                value.exprs.push(Expr {
                    kind: ExprKind::Register(value_token_kind),
                    exprs: vec![],
                });
                return Ok(Some(value));
            }
            _ => (),
        };

        let expr = self.expression();
        match expr {
            Ok(Some(expr)) => value.exprs.push(expr),
            Ok(None) => return Ok(None),
            Err(e) => return Err(e),
        };
        Ok(Some(value))
    }

    fn expression(&mut self) -> Result<Option<Expr>, String> {
        let expr_token_kind = match self.peek() {
            Some(t) => t.as_tuple().0,
            None => return Ok(None),
        };

        let next_isnt_colon = match self.tokens.get(self.index + 1) {
            Some(x) if matches!(x.0, TokenKind::Colon) => false,
            _ => true,
        };

        let kind = match expr_token_kind {
            TokenKind::Integer(v) => ExprKind::Literal(v),
            TokenKind::Label(n) if next_isnt_colon => ExprKind::Label(n),
            _ => return Ok(None),
        };

        self.next();

        Ok(Some(Expr {
            kind,
            exprs: vec![],
        }))
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
        };

        self.next();

        loop {
            match self.expression() {
                Ok(Some(expr)) => directive.exprs.push(expr),
                Ok(None) => break,
                Err(e) => return Err(e),
            };
        }

        Ok(Some(directive))
    }

    fn label(&mut self) -> Result<Option<Expr>, String> {
        let label_token = match self.peek() {
            Some(t) => t,
            None => return Ok(None),
        };

        let kind = match &label_token.0 {
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
            _ => return Err(format!("No ':' after label on line {}", colon_token.2)),
        };

        self.next();

        Ok(Some(Expr {
            kind,
            exprs: vec![],
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
