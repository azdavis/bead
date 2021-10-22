//! Parse tokens into a parse tree.

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]
#![deny(unsafe_code)]

mod expr;
mod ty;

use event_parse::rowan_sink::{Error, RowanSink};
use event_parse::Parser;
use syntax::{ast::Root, SyntaxKind as SK, Token};

/// Does the parsing.
pub fn get(tokens: &[Token<'_, SK>]) -> Parse {
  let mut p = Parser::new(tokens);
  let en = p.enter();
  while p.peek().is_some() {
    expr::expr(&mut p);
  }
  p.exit(en, SK::Root);
  let mut sink = RowanSink::default();
  p.finish(&mut sink);
  let (node, errors) = sink.finish();
  Parse {
    root: Root::try_from(node).expect("there is exactly one root"),
    errors,
  }
}

/// The result of parsing.
#[derive(Debug)]
pub struct Parse {
  /// The root.
  pub root: Root,
  /// The errors.
  pub errors: Vec<Error<SK>>,
}
