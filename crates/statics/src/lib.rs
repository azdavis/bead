//! Bidirectional typechecking for arbitrary-rank types.
//!
//! Mostly adapted from [this MSR paper][1].
//!
//! [1]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/putting.pdf

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]

mod defs;
mod uniq;

pub use defs::*;
