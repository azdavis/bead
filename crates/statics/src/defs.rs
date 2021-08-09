//! Definitions of data types.

use hir::Name;
use rustc_hash::FxHashMap;
use uniq::{Uniq, UniqGen};

/// A type. "Sigma" in the MSR paper.
#[derive(Debug, Clone)]
#[allow(clippy::enum_variant_names)]
pub enum Ty {
  /// "No" type.
  None,
  /// Forall types. The bound type variables may appear in the [`Rho`].
  ///
  /// Prefer using [`Ty::for_all`] to construct these.
  ForAll(Vec<BoundTyVar>, Box<Rho>),
  /// Functions, from arguments to results.
  ///
  /// Prefer using [`Ty::fun`] to construct these.
  Fun(Box<Ty>, Box<Ty>),
  /// Integer.
  Int,
  /// Type variables.
  TyVar(TyVar),
  /// Meta type variables.
  MetaTyVar(MetaTyVar),
}

impl Ty {
  /// Returns a function type from `arg` to `res`.
  pub(crate) fn fun(arg: Self, res: Self) -> Self {
    Self::Fun(Box::new(arg), Box::new(res))
  }

  /// Returns a forall type, unless `tvs` is empty, in which case this returns
  /// the [`Ty`] inside `rho`.
  pub(crate) fn for_all(tvs: Vec<BoundTyVar>, rho: Rho) -> Self {
    if tvs.is_empty() {
      rho.into_inner()
    } else {
      Self::ForAll(tvs, Box::new(rho))
    }
  }
}

/// A [`Ty`] that is not [`Ty::ForAll`] at the top level, but may contain them
/// nested inside.
#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct Rho(Ty);

impl Rho {
  /// Returns a new [`Rho`]. Panics in debug mode if it is a [`Ty::ForAll`].
  pub(crate) fn new(ty: Ty) -> Self {
    debug_assert!(Self::is_valid(&ty));
    Self(ty)
  }

  /// Returns a new [`Rho`] if the `ty` is not a [`Ty::ForAll`].
  pub(crate) fn new_opt(ty: Ty) -> Option<Self> {
    Self::is_valid(&ty).then(|| Self(ty))
  }

  fn is_valid(ty: &Ty) -> bool {
    !matches!(ty, Ty::ForAll(_, _))
  }

  /// Unwraps the [`Ty`].
  pub(crate) fn into_inner(self) -> Ty {
    self.0
  }
}

impl AsRef<Ty> for Rho {
  fn as_ref(&self) -> &Ty {
    &self.0
  }
}

/// A [`Ty`] with no [`Ty::ForAll`] at all, i.e. a monotype.
#[derive(Debug, Clone)]
#[repr(transparent)]
pub(crate) struct Tau(Ty);

impl Tau {
  /// Returns a new [`Tau`]. Panics in debug mode if it contains [`Ty::ForAll`].
  pub(crate) fn new(ty: Ty) -> Self {
    debug_assert!(Self::is_valid(&ty));
    Self(ty)
  }

  fn is_valid(ty: &Ty) -> bool {
    match ty {
      Ty::ForAll(_, _) => false,
      Ty::Fun(arg_ty, res_ty) => {
        Self::is_valid(arg_ty) && Self::is_valid(res_ty)
      }
      Ty::None | Ty::Int | Ty::TyVar(_) | Ty::MetaTyVar(_) => true,
    }
  }

  /// Unwraps the [`Ty`].
  pub(crate) fn into_inner(self) -> Ty {
    self.0
  }
}

impl AsRef<Ty> for Tau {
  fn as_ref(&self) -> &Ty {
    &self.0
  }
}

/// A type variable.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyVar {
  /// A bound type variable.
  Bound(BoundTyVar),
  /// A skolem type variable.
  Skolem(SkolemTyVar),
}

/// A type variable bound by a [`Ty::ForAll`]. This is the only kind of type
/// variable writeable in user code.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BoundTyVar(Name);

impl BoundTyVar {
  /// Returns a new [`BoundTyVar`].
  pub(crate) fn new(name: Name) -> Self {
    Self(name)
  }
}

/// A skolem type variable. Not bound a [`Ty::ForAll`]. A constant but unknown
/// type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SkolemTyVar(Uniq);

/// A meta type variable. Not bound a [`Ty::ForAll`]. A placeholder for a
/// monotype, which is to be determined by type inference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MetaTyVar(Uniq);

/// Mutable state updated during typechecking.
#[derive(Debug, Default)]
pub(crate) struct Cx {
  uniq_gen: UniqGen,
  skolem_names: FxHashMap<SkolemTyVar, Name>,
  meta_tys: FxHashMap<MetaTyVar, Tau>,
}

impl Cx {
  /// Returns a new [`MetaTyVar`] distinct from any other returned thus far.
  pub(crate) fn new_meta_ty_var(&mut self) -> MetaTyVar {
    MetaTyVar(self.uniq_gen.gen())
  }

  /// Returns a new [`SkolemTyVar`] distinct from any other returned thus far.
  pub(crate) fn new_skolem_ty_var(&mut self, tv: TyVar) -> SkolemTyVar {
    let ret = SkolemTyVar(self.uniq_gen.gen());
    let name = self.ty_var_name(tv);
    assert!(self.skolem_names.insert(ret, name).is_none());
    ret
  }

  fn ty_var_name(&self, tv: TyVar) -> Name {
    match tv {
      TyVar::Bound(BoundTyVar(name)) => name,
      TyVar::Skolem(tv) => self.skolem_names[&tv].clone(),
    }
  }

  /// Sets `tv` to refer to `ty`. Panics if `tv` already refers to some other
  /// [`Tau`].
  pub(crate) fn set(&mut self, tv: MetaTyVar, ty: Tau) {
    assert!(self.meta_tys.insert(tv, ty).is_none());
  }

  /// Sets `tv` to refer to `ty`. Panics if `tv` did *not* already refer to some
  /// other [`Tau`].
  pub(crate) fn reset(&mut self, tv: MetaTyVar, ty: Tau) {
    assert!(self.meta_tys.insert(tv, ty).is_some());
  }

  /// Returns the [`Tau`] that `tv` refers to, if any.
  pub(crate) fn get(&self, tv: MetaTyVar) -> Option<&Tau> {
    self.meta_tys.get(&tv)
  }
}

/// Variable names in scope and their types.
#[derive(Debug, Default, Clone)]
pub(crate) struct Env(FxHashMap<Name, Ty>);

impl Env {
  /// Insert `name` as having `ty`.
  pub(crate) fn insert(mut self, name: Name, ty: Ty) -> Self {
    self.0.insert(name, ty);
    self
  }

  /// Returns the [`Ty`] that `name` refers to, if any.
  pub(crate) fn get(&self, name: &Name) -> Option<&Ty> {
    self.0.get(name)
  }

  /// Returns an iterator over the values.
  pub(crate) fn values(&self) -> impl Iterator<Item = &Ty> {
    self.0.values()
  }
}

/// A slot for a [`Rho`] that starts empty and may be set exactly once.
#[derive(Debug, Default)]
pub(crate) struct RhoRef(Option<Rho>);

impl RhoRef {
  /// Sets the [`RhoRef`] if its unset. Panics if it is set.
  pub(crate) fn set(&mut self, rho: Rho) {
    assert!(self.0.is_none());
    self.0 = Some(rho);
  }

  /// Unwraps the [`Rho`] inside, if any. Panics if there is none.
  pub(crate) fn unwrap(self) -> Rho {
    self.0.unwrap()
  }
}
