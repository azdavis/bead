//! Unique identifiers.

/// A unique identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct Uniq(u32);

/// A generator for [`Uniq`]s.
#[derive(Debug, Default)]
pub(crate) struct UniqGen(u32);

impl UniqGen {
  /// Returns a [`Uniq`] not equal to any other [`Uniq`] returned thus far from
  /// this [`UniqGen`].
  pub(crate) fn gen(&mut self) -> Uniq {
    let ret = Uniq(self.0);
    // assuming overflow won't happen.
    self.0 += 1;
    ret
  }
}
