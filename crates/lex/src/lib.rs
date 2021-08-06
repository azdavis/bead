//! Lex a string into tokens.

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]
#![deny(unsafe_code)]

use syntax::{SyntaxKind as SK, Token};

/// Does the lexing.
pub fn get(s: &str) -> Lex<'_> {
  let mut tokens = Vec::new();
  let mut cx = Cx {
    i: 0,
    bs: s.as_bytes(),
    errors: Vec::new(),
  };
  while let Some(&b) = cx.bs.get(cx.i) {
    let start = cx.i;
    let kind = go(&mut cx, b);
    assert!(start < cx.i);
    let text = std::str::from_utf8(&cx.bs[start..cx.i]).unwrap();
    tokens.push(Token { kind, text });
  }
  Lex {
    tokens,
    errors: cx.errors,
  }
}

/// The result of lexing.
#[derive(Debug)]
pub struct Lex<'a> {
  /// The tokens.
  pub tokens: Vec<Token<'a, SK>>,
  /// The errors.
  pub errors: Vec<Error>,
}

/// An error.
#[derive(Debug)]
pub enum Error {
  /// Some source text was invalid.
  InvalidSource,
}

#[derive(Debug)]
struct Cx<'a> {
  i: usize,
  bs: &'a [u8],
  errors: Vec<Error>,
}

fn go(cx: &mut Cx<'_>, b: u8) -> SK {
  // whitespace
  if b.is_ascii_whitespace() {
    advance_while(cx, |b| b.is_ascii_whitespace());
    return SK::Whitespace;
  }
  // line comments
  if b == b'/' && cx.bs.get(cx.i + 1) == Some(&b'/') {
    cx.i += 2;
    advance_while(cx, |b| b != b'\n');
    return SK::LineComment;
  }
  // integers
  if b.is_ascii_digit() {
    advance_while(cx, |b| b.is_ascii_digit());
    return SK::IntLit;
  }
  // keywords, names
  if b.is_ascii_alphabetic() {
    let start = cx.i;
    advance_while(cx, |b| b.is_ascii_alphabetic() || b == b'_');
    return SK::keyword(&cx.bs[start..cx.i]).unwrap_or(SK::Name);
  }
  // punctuation
  for &(tok_bs, tok) in SK::PUNCTUATION.iter() {
    if cx.bs.get(cx.i..cx.i + tok_bs.len()) == Some(tok_bs) {
      cx.i += tok_bs.len();
      return tok;
    }
  }
  // invalid
  let start = cx.i;
  loop {
    cx.i += 1;
    let s = cx.bs.get(start..cx.i);
    if s.map_or(true, |s| std::str::from_utf8(s).is_ok()) {
      cx.errors.push(Error::InvalidSource);
      return SK::Invalid;
    }
  }
}

fn advance_while(cx: &mut Cx<'_>, f: fn(u8) -> bool) {
  while let Some(&b) = cx.bs.get(cx.i) {
    if f(b) {
      cx.i += 1;
    } else {
      break;
    }
  }
}
