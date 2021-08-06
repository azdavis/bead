//! Concrete syntax.

#![deny(rust_2018_idioms)]

pub mod ast;
mod kind;

pub use kind::*;
pub use token::*;
