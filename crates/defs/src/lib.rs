//! Definitions of data types.

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]
#![deny(unsafe_code)]

use std::collections::HashMap;
use uniq::{Uniq, UniqGen};

/// A name in code, aka a variable, an identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Name(Uniq);

impl Name {
  /// Returns a new [`Name`].
  pub fn new(u: Uniq) -> Self {
    Self(u)
  }
}

/// An expression.
#[derive(Debug)]
pub enum Expr {
  /// Integer literal.
  Int(i32),
  /// Variable.
  Var(Name),
  /// Lambda, a function literal.
  Lam(Name, Box<Expr>),
  /// Annotated lambda. The variable type is given by the [`Ty`].
  ALam(Name, Ty, Box<Expr>),
  /// Function application.
  App(Box<Expr>, Box<Expr>),
  /// Variable binding. The variable's contents are the first [`Expr`] and it
  /// is is available in the scope of the latter [`Expr`].
  Let(Name, Box<Expr>, Box<Expr>),
  /// Annotation.
  Ann(Box<Expr>, Ty),
}

/// A type. "Sigma" in the MSR paper.
#[derive(Debug, Clone)]
pub enum Ty {
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
  pub fn fun(arg: Self, res: Self) -> Self {
    Self::Fun(Box::new(arg), Box::new(res))
  }

  /// Returns a forall type, unless `tvs` is empty, in which case this returns
  /// the [`Ty`] inside `rho`.
  pub fn for_all(tvs: Vec<BoundTyVar>, rho: Rho) -> Self {
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
pub struct Rho(Ty);

impl Rho {
  /// Returns a new [`Rho`]. Panics in debug mode if it is a [`Ty::ForAll`].
  pub fn new(ty: Ty) -> Self {
    #[cfg(debug_assertions)]
    Self::check(&ty);
    Self(ty)
  }

  fn check(ty: &Ty) {
    if let Ty::ForAll(_, _) = ty {
      panic!("top-level ForAll in Rho: {:?}", ty);
    }
  }

  /// Unwraps the [`Ty`].
  pub fn into_inner(self) -> Ty {
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
pub struct Tau(Ty);

impl Tau {
  /// Returns a new [`Tau`]. Panics in debug mode if it contains [`Ty::ForAll`].
  pub fn new(ty: Ty) -> Self {
    #[cfg(debug_assertions)]
    Self::check(&ty);
    Self(ty)
  }

  fn check(ty: &Ty) {
    match ty {
      Ty::ForAll(_, _) => panic!("ForAll in Tau: {:?}", ty),
      Ty::Fun(arg_ty, res_ty) => {
        Self::check(arg_ty);
        Self::check(res_ty);
      }
      Ty::Int | Ty::TyVar(_) | Ty::MetaTyVar(_) => {}
    }
  }

  /// Unwraps the [`Ty`].
  pub fn into_inner(self) -> Ty {
    self.0
  }
}

impl AsRef<Ty> for Tau {
  fn as_ref(&self) -> &Ty {
    &self.0
  }
}

/// A type variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TyVar {
  /// A bound type variable.
  Bound(BoundTyVar),
  /// A skolem type variable.
  Skolem(SkolemTyVar),
}

/// A type variable bound by a [`Ty::ForAll`]. This is the only kind of type
/// variable writeable in user code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BoundTyVar(Name);

impl BoundTyVar {
  /// Returns a new [`BoundTyVar`].
  pub fn new(name: Name) -> Self {
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
pub struct Cx {
  uniq_gen: UniqGen,
  skolem_names: HashMap<SkolemTyVar, Name>,
  meta_tys: HashMap<MetaTyVar, Tau>,
}

impl Cx {
  /// Returns a new [`MetaTyVar`] distinct from any other returned thus far.
  pub fn new_meta_ty_var(&mut self) -> MetaTyVar {
    MetaTyVar(self.uniq_gen.gen())
  }

  /// Returns a new [`SkolemTyVar`] distinct from any other returned thus far.
  pub fn new_skolem_ty_var(&mut self, tv: TyVar) -> SkolemTyVar {
    let ret = SkolemTyVar(self.uniq_gen.gen());
    let name = self.ty_var_name(tv);
    assert!(self.skolem_names.insert(ret, name).is_none());
    ret
  }

  fn ty_var_name(&self, tv: TyVar) -> Name {
    match tv {
      TyVar::Bound(BoundTyVar(name)) => name,
      TyVar::Skolem(tv) => self.skolem_names[&tv],
    }
  }

  /// Sets `tv` to refer to `ty`. Panics if `tv` already refers to some other
  /// [`Tau`].
  pub fn set(&mut self, tv: MetaTyVar, ty: Tau) {
    assert!(self.meta_tys.insert(tv, ty).is_none());
  }

  /// Sets `tv` to refer to `ty`. Panics if `tv` did *not* already refer to some
  /// other [`Tau`].
  pub fn reset(&mut self, tv: MetaTyVar, ty: Tau) {
    assert!(self.meta_tys.insert(tv, ty).is_some());
  }

  /// Returns the [`Tau`] that `tv` refers to, if any.
  pub fn get(&self, tv: MetaTyVar) -> Option<&Tau> {
    self.meta_tys.get(&tv)
  }
}

/// Variable names in scope and their types.
#[derive(Debug, Default, Clone)]
pub struct Env(HashMap<Name, Ty>);

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

/// A slot for a [`Rho`] that starts empty and may be set exactly once.
#[derive(Debug, Default)]
pub struct RhoRef(Option<Rho>);

impl RhoRef {
  /// Sets the [`RhoRef`] if its unset. Panics if it is set.
  pub fn set(&mut self, rho: Rho) {
    assert!(self.0.is_none());
    self.0 = Some(rho);
  }

  /// Unwraps the [`Rho`] inside, if any. Panics if there is none.
  pub fn unwrap(self) -> Rho {
    self.0.unwrap()
  }
}
