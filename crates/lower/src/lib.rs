//! Lower concrete syntax into abstract.

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]
#![deny(unsafe_code)]

mod expr;
mod ty;
mod util;

pub use util::Lower;

/// Does the lowering.
pub fn get(root: syntax::ast::Root) -> Lower {
  let mut cx = Lower::default();
  for expr in root.exprs() {
    let e = expr::get(&mut cx, expr);
    cx.top.push(e);
  }
  cx
}
