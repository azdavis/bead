//! Lower concrete syntax into abstract.

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]
#![deny(unsafe_code)]

mod expr;
mod ty;
mod util;

use defs::{Expr, Name};
use std::collections::HashMap;
use syntax::ast::Root;

/// Does the lowering.
pub fn get(root: Root) -> Lower {
  let mut cx = util::Cx::default();
  let exprs: Vec<_> =
    root.exprs().filter_map(|e| expr::get(&mut cx, e)).collect();
  Lower {
    exprs,
    names: cx.finish(),
  }
}

/// The result of lowering.
#[derive(Debug)]
pub struct Lower {
  /// The expressions.
  pub exprs: Vec<Expr>,
  /// A map from each name to the string it represents.
  pub names: HashMap<Name, String>,
}
