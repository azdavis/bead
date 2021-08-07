//! Bidirectional typechecking for arbitrary-rank types.
//!
//! Mostly adapted from [this MSR paper][1].
//!
//! [1]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/putting.pdf

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]
#![deny(unsafe_code)]

mod expr;
mod ty;

/// Infer a type for `expr` under `env`.
pub fn get(expr: &defs::Expr) -> defs::Ty {
  let mut cx = defs::Cx::default();
  let env = defs::Env::default();
  expr::infer_ty_zonk(&mut cx, &env, expr)
}
