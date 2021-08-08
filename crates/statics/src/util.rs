use defs::Ty;
use hir::Name;
use rustc_hash::FxHashMap;

/// Variable names in scope and their types.
#[derive(Debug, Default, Clone)]
pub struct Env(FxHashMap<Name, Ty>);

impl Env {
  /// Insert `name` as having `ty`.
  pub fn insert(mut self, name: Name, ty: Ty) -> Self {
    self.0.insert(name, ty);
    self
  }

  /// Returns the [`Ty`] that `name` refers to, if any.
  pub fn get(&self, name: &Name) -> Option<&Ty> {
    self.0.get(name)
  }

  /// Returns an iterator over the values.
  pub fn values(&self) -> impl Iterator<Item = &Ty> {
    self.0.values()
  }
}
