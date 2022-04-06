//! Bidirectional typechecking for arbitrary-rank types.
//!
//! Mostly adapted from [this MSR paper][1].
//!
//! [1]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/putting.pdf

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]
#![deny(unsafe_code)]

mod defs;
mod expr;
mod lower;
mod ty;

/// Infer a type for `expr` under `env`.
pub fn get(arenas: &hir::Arenas, expr: hir::ExprIdx) -> defs::Statics {
  let mut cx = defs::Cx::default();
  let env = defs::Env::default();
  let ty = expr::infer_ty(&mut cx, arenas, &env, expr);
  defs::Statics {
    ty,
    errors: cx.finish(),
  }
}
