use super::parser::{Expr, ExprKind};

pub struct CodeGen {
    expressions: Vec<Expr>,
    index: usize,
}
