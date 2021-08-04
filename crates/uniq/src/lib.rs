//! Unique identifiers.

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]
#![deny(unsafe_code)]
#![no_std]

/// A unique identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Uniq(u32);

/// A generator for [`Uniq`]s.
#[derive(Debug, Default)]
pub struct UniqGen(u32);

impl UniqGen {
  /// Returns a [`Uniq`] not equal to any other [`Uniq`] returned thus far from
  /// this [`UniqGen`].
  pub fn gen(&mut self) -> Uniq {
    let ret = Uniq(self.0);
    // assuming overflow won't happen.
    self.0 += 1;
    ret
  }
}
