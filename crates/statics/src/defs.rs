//! Definitions of data types.

use crate::uniq::{Uniq, UniqGen};
use std::collections::HashMap;

/// A name in code, aka a variable, an identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Name(u32);

impl Name {
  pub(crate) fn new(n: u32) -> Self {
    Self(n)
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
  /// Annotated lambda. The variable type is given by the [`Sigma`].
  ALam(Name, Sigma, Box<Expr>),
  /// Function application.
  App(Box<Expr>, Box<Expr>),
  /// Variable binding. The variable's contents are the first [`Expr`] and it
  /// is is available in the scope of the latter [`Expr`].
  Let(Name, Box<Expr>, Box<Expr>),
  /// Annotation.
  Ann(Box<Expr>, Sigma),
}

/// All types.
pub type Sigma = Ty;

/// No top-level [`Ty::ForAll`].
pub type Rho = Ty;

/// No [`Ty::ForAll`] at all, i.e. a monotype.
pub type Tau = Ty;

/// A type.
#[derive(Debug, Clone)]
pub enum Ty {
  /// Forall types. The bound type variables may appear in the [`Rho`].
  ForAll(Vec<BoundTyVar>, Box<Rho>),
  /// Functions, from arguments to results.
  Fun(Box<Ty>, Box<Ty>),
  /// Integer.
  Int,
  /// Type variables.
  TyVar(TyVar),
  /// Meta type variables.
  MetaTyVar(MetaTyVar),
}

impl Ty {
  pub(crate) fn fun(ty1: Self, ty2: Self) -> Self {
    Self::Fun(Box::new(ty1), Box::new(ty2))
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

/// Bound by a [`Ty::ForAll`]. This is the only kind of type variable writeable
/// in user code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BoundTyVar(Name);

impl BoundTyVar {
  pub(crate) fn new(name: Name) -> Self {
    Self(name)
  }
}

/// Not bound a [`Ty::ForAll`]. A constant but unknown type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SkolemTyVar(Uniq);

/// Not bound a [`Ty::ForAll`]. A placeholder for a monotype, which is to be
/// determined by type inference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MetaTyVar(Uniq);

/// Mutable state updated during typechecking.
#[derive(Debug)]
pub struct Cx {
  uniq_gen: UniqGen,
  skolem_names: HashMap<SkolemTyVar, Name>,
  meta_tys: HashMap<MetaTyVar, Tau>,
}

impl Cx {
  pub(crate) fn new_meta_ty_var(&mut self) -> MetaTyVar {
    MetaTyVar(self.uniq_gen.gen())
  }

  pub(crate) fn new_skolem_ty_var(&mut self, tv: TyVar) -> SkolemTyVar {
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

  pub(crate) fn set(&mut self, tv: MetaTyVar, ty: Tau) {
    assert!(self.meta_tys.insert(tv, ty).is_none());
  }

  pub(crate) fn get(&self, tv: MetaTyVar) -> Option<&Tau> {
    self.meta_tys.get(&tv)
  }
}

/// Variable names in scope and their types.
#[derive(Debug, Default, Clone)]
pub struct Env(HashMap<Name, Sigma>);

impl Env {
  pub(crate) fn insert(mut self, name: Name, ty: Sigma) -> Self {
    self.0.insert(name, ty);
    self
  }

  pub(crate) fn get(&self, name: &Name) -> Option<&Sigma> {
    self.0.get(name)
  }

  pub(crate) fn values(&self) -> impl Iterator<Item = &Sigma> {
    self.0.values()
  }
}

/// A slot for a [`Rho`] that starts empty and may be set exactly once.
#[derive(Debug, Default)]
pub(crate) struct RhoRef(Option<Rho>);

impl RhoRef {
  pub(crate) fn set(&mut self, rho: Rho) {
    assert!(self.0.is_none());
    self.0 = Some(rho);
  }

  pub(crate) fn unwrap(self) -> Rho {
    self.0.unwrap()
  }
}
