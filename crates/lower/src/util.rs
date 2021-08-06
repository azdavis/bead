use defs::Name;
use std::collections::HashMap;
use uniq::UniqGen;

#[derive(Debug, Default)]
pub(crate) struct Cx {
  uniq_gen: UniqGen,
  names: HashMap<String, Name>,
}

impl Cx {
  pub(crate) fn name(&mut self, s: &str) -> Name {
    if let Some(&name) = self.names.get(s) {
      return name;
    }
    let ret = Name::new(self.uniq_gen.gen());
    self.names.insert(s.to_owned(), ret);
    ret
  }

  /// TODO could be more efficient by using a vec map or something, since name
  /// is just an index.
  pub(crate) fn finish(self) -> HashMap<Name, String> {
    self.names.into_iter().map(|(s, n)| (n, s)).collect()
  }
}
