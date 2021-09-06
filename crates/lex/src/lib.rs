//! Lex a string into tokens.

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]
#![deny(unsafe_code)]

use std::convert::TryInto as _;
use std::fmt;
use syntax::{SyntaxKind as SK, TextRange, TextSize, Token};

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
    let text = std::str::from_utf8(&cx.bs[start..cx.i])
      .expect("text of every token should be str");
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
pub struct Error {
  /// The range.
  pub range: TextRange,
  /// The error kind.
  pub kind: ErrorKind,
}

/// An error kind.
#[derive(Debug)]
pub enum ErrorKind {
  /// Some source text was invalid.
  InvalidSource,
}

impl fmt::Display for ErrorKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      ErrorKind::InvalidSource => f.write_str("invalid source"),
    }
  }
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
    cx.i += 1;
    advance_while(cx, |b| b.is_ascii_digit() || b == b'_');
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
      err(cx, start, ErrorKind::InvalidSource);
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

fn err(cx: &mut Cx<'_>, start: usize, kind: ErrorKind) {
  cx.errors.push(Error {
    range: range(start, cx.i),
    kind,
  })
}

fn range(start: usize, end: usize) -> TextRange {
  TextRange::new(text_size(start), text_size(end))
}

fn text_size(n: usize) -> TextSize {
  n.try_into().expect("n too large for TextSize")
}
