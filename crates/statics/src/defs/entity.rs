use hir::{ExprIdx, TyIdx};

/// An entity that can be given an error.
#[derive(Debug, Clone, Copy)]
pub enum Entity {
  Expr(ExprIdx),
  Ty(TyIdx),
}
