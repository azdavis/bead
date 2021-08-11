//! Lower concrete syntax into abstract.

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]
#![deny(unsafe_code)]

mod defs;
mod expr;
mod ty;

pub use defs::{Lower, Ptrs};

/// Does the lowering.
pub fn get(root: syntax::ast::Root) -> Lower {
  let mut cx = Lower::default();
  for expr in root.exprs() {
    let e = expr::get(&mut cx, expr);
    cx.top.push(e);
  }
  cx
}
