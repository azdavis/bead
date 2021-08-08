use hir::la_arena::ArenaMap;
use hir::{Arenas, ExprIdx, TyIdx};
use rustc_hash::FxHashMap;
use syntax::ast::{Expr, Ty};
use syntax::AstPtr;

/// The result of lowering.
#[derive(Debug, Default)]
pub struct Lower {
  /// The arenas.
  pub arenas: Arenas,
  /// The pointers.
  pub ptrs: Ptrs,
  /// The top-level expressions.
  pub top: Vec<ExprIdx>,
}

/// The pointers.
#[derive(Debug, Default)]
pub struct Ptrs {
  /// A map forward from HIR to AST expressions.
  pub expr_fwd: ArenaMap<ExprIdx, AstPtr<Expr>>,
  /// A map back from AST to HIR expressions.
  pub expr_back: FxHashMap<AstPtr<Expr>, ExprIdx>,
  /// A map forward from HIR to AST types.
  pub ty_fwd: ArenaMap<TyIdx, AstPtr<Ty>>,
  /// A map back from AST to HIR types.
  pub ty_back: FxHashMap<AstPtr<Ty>, TyIdx>,
}
