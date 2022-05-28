use super::lexer::{Token, TokenKind};
use std::fmt;

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

#[derive(Debug)]
enum CondKind {
    Eq,
    Neq,
    Lt,
    Gte,
    Gt,
    Lte,
    Cr,
    Ncr,
    None,
}

#[derive(Debug)]
#[allow(dead_code)]
enum ExprKind {
    Instruction(CondKind),
    Op,
    Value,
    Expression,
    Literal(i16),
    Unary,
    Binary,
    Grouping,
    Directive,
}

pub struct Expr {
    kind: ExprKind,
    exprs: Vec<Expr>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, index: 0 }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.index)
    }

    fn next(&mut self) -> Option<&Token> {
        match self.tokens.get(self.index) {
            Some(t) => {
                self.index += 1;
                Some(t)
            }
            None => None,
        }
    }

    /*
     * instruction = op | op "?" CONDITION
     * op          = OPCODE | OPCODE value | OPCODE value value
     * value       = REGISTER | expression
     * expression  = literal | unary | binary | grouping
     * literal     = INTEGER
     * unary       = OPERATOR_UNARY expression
     * binary      = expression OPERATOR_BINARY expression
     * grouping    = "(" expression ")"
     *
     * directive   = DIRECTIVE (expression)* | DIRECTIVE (STRING)*
     */

    pub fn parse_one_statement(&mut self) -> Result<Option<Expr>, String> {
        let expr = if let Ok(Some(instruction_res)) = self.instruction() {
            instruction_res
        } else if let Ok(Some(directive_res)) = self.directive() {
            directive_res
        } else {
            return Ok(None);
        };

        Ok(Some(expr))
    }

    fn instruction(&mut self) -> Result<Option<Expr>, String> {
        // Ensure that there's an operator to be read in
        let mut op = match self.op() {
            Ok(Some(op)) => op,
            Ok(None) => return Ok(None),
            Err(e) => return Err(e),
        };

        // Change to an instruciton to be returned with no condition by default
        op.change_kind(ExprKind::Instruction(CondKind::None));

        // Peek for next token
        let peek = match self.peek() {
            Some(t) => t,
            None => return Ok(Some(op)),
        };

        // Check if there's a '?' for the conditional
        if !matches!(peek.0, TokenKind::QuestionMark) {
            return Ok(Some(op)); // If there's none, just return the op with no condition as an instruction
        }

        let current_line = peek.2;

        self.next(); // Consume the '?'

        // Get the next word for the condition if it exists
        let peek = match self.peek() {
            Some(t) => t,
            None => return Err(format!("No condition after '?' on line {}", current_line)),
        };

        // Check that the peeked token is in fact a condition, and if so, set that to op's cond
        match CondKind::from_token_kind(peek.0.to_owned()) {
            Some(k) => op.change_kind(ExprKind::Instruction(k)),
            None => {
                return Err(format!(
                    "No condition after the '?' on line {}",
                    current_line
                ))
            }
        }

        Ok(Some(op))
    }

    fn op(&mut self) -> Result<Option<Expr>, String> {
        //match self.peek() {

        //}
        todo!()
    }

    //TODO:

    // fn value(&mut self) -> Result<Option<Expr>, String> {}

    // fn expression(&mut self) -> Result<Option<Expr>, String> {}

    // fn literal(&mut self) -> Result<Option<Expr>, String> {}

    // fn unary(&mut self) -> Result<Option<Expr>, String> {}

    // fn binary(&mut self) -> Result<Option<Expr>, String> {}

    // fn grouping(&mut self) -> Result<Option<Expr>, String> {}

    fn directive(&mut self) -> Result<Option<Expr>, String> {
        Ok(Some(Expr {
            kind: ExprKind::Directive,
            exprs: vec![],
        }))
    }
}

impl CondKind {
    fn from_token_kind(token_kind: TokenKind) -> Option<Self> {
        let kind = match token_kind {
            TokenKind::Eq => CondKind::Eq,
            TokenKind::Neq => CondKind::Neq,
            TokenKind::Lt => CondKind::Lt,
            TokenKind::Gte => CondKind::Gte,
            TokenKind::Gt => CondKind::Gt,
            TokenKind::Lte => CondKind::Lte,
            TokenKind::Cr => CondKind::Cr,
            TokenKind::Ncr => CondKind::Ncr,
            _ => return None,
        };

        Some(kind)
    }
}

impl Expr {
    fn change_kind(&mut self, kind: ExprKind) {
        self.kind = kind;
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({:?}) {:?}", self.kind, self.exprs)
    }
}
