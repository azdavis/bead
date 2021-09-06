//! Definitions of data types.

mod entity;

pub use entity::Entity;
use hir::Name;
use rustc_hash::FxHashMap;
use std::fmt;
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

  fn fmt_prec(&self, f: &mut fmt::Formatter<'_>, prec: TyPrec) -> fmt::Result {
    match self {
      Ty::None => f.write_str("_"),
      Ty::ForAll(tvs, ty) => {
        if prec < TyPrec::A {
          f.write_str("(")?;
        }
        f.write_str("forall")?;
        for tv in tvs {
          write!(f, " {}", tv)?;
        }
        f.write_str(". ")?;
        (**ty).as_ref().fmt_prec(f, TyPrec::A)?;
        if prec < TyPrec::A {
          f.write_str(")")?;
        }
        Ok(())
      }
      Ty::Fun(arg, res) => {
        if prec < TyPrec::A {
          f.write_str("(")?;
        }
        arg.fmt_prec(f, TyPrec::B)?;
        f.write_str(" -> ")?;
        res.fmt_prec(f, TyPrec::A)?;
        if prec < TyPrec::A {
          f.write_str(")")?;
        }
        Ok(())
      }
      Ty::Int => f.write_str("Int"),
      Ty::TyVar(tv) => write!(f, "{}", tv),
      Ty::MetaTyVar(tv) => write!(f, "{}", tv),
    }
  }
}

impl fmt::Display for Ty {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.fmt_prec(f, TyPrec::A)
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum TyPrec {
  B,
  A,
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

impl fmt::Display for Rho {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.0.fmt(f)
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

impl fmt::Display for TyVar {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      TyVar::Bound(tv) => tv.fmt(f),
      TyVar::Skolem(tv) => tv.fmt(f),
    }
  }
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

impl fmt::Display for BoundTyVar {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.0.fmt(f)
  }
}

/// A skolem type variable. Not bound a [`Ty::ForAll`]. A constant but unknown
/// type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SkolemTyVar(Uniq);

impl fmt::Display for SkolemTyVar {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    // TODO show the Uniq?
    f.write_str("_")
  }
}

/// A meta type variable. Not bound a [`Ty::ForAll`]. A placeholder for a
/// monotype, which is to be determined by type inference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MetaTyVar(Uniq);

impl fmt::Display for MetaTyVar {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "${}", self.0)
  }
}

/// An error.
#[derive(Debug)]
pub struct Error {
  /// The entity.
  pub entity: Entity,
  /// The kind.
  pub kind: ErrorKind,
}

/// A kind of error.
#[derive(Debug)]
pub enum ErrorKind {
  NotPolymorphicEnough(Ty),
  /// the first is not as polymorphic as the second.
  NotAsPolymorphicAsOther(Ty, Ty),
  CannotUnify(Ty, Ty),
  OccursCheckFailed(Ty, MetaTyVar),
  NotInScope(Name),
  InvalidRhoTy,
}

impl fmt::Display for ErrorKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      ErrorKind::NotPolymorphicEnough(ty) => {
        write!(f, "{} is not polymorphic enough", ty)
      }
      ErrorKind::NotAsPolymorphicAsOther(ty1, ty2) => {
        write!(f, "{} is not as polymorphic as {}", ty1, ty2)
      }
      ErrorKind::CannotUnify(ty1, ty2) => {
        write!(f, "mismatched types: cannot unify {} with {}", ty1, ty2)
      }
      ErrorKind::OccursCheckFailed(ty, tv) => {
        write!(f, "type variable {} occurs in {}", tv, ty)
      }
      ErrorKind::NotInScope(name) => write!(f, "name {} is not in scope", name),
      ErrorKind::InvalidRhoTy => write!(f, "invalid rho type"),
    }
  }
}

/// Mutable state updated during typechecking.
#[derive(Debug, Default)]
pub(crate) struct Cx {
  uniq_gen: UniqGen,
  skolem_names: FxHashMap<SkolemTyVar, Name>,
  meta_tys: FxHashMap<MetaTyVar, Tau>,
  errors: Vec<Error>,
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

  /// Records an error.
  pub(crate) fn err(&mut self, entity: Entity, kind: ErrorKind) {
    self.errors.push(Error { entity, kind });
  }

  /// Returns the errors.
  pub(crate) fn finish(self) -> Vec<Error> {
    self.errors
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

  /// Unwraps the [`Rho`] inside, if any. Panics with the message if there is
  /// none.
  pub(crate) fn expect(self, message: &str) -> Rho {
    self.0.expect(message)
  }
}

/// The result of running statics.
#[derive(Debug)]
pub struct Statics {
  /// The type of the expression.
  pub ty: Ty,
  /// The errors.
  pub errors: Vec<Error>,
}
