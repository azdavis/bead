//! High-level Intermediate Representation.

#![deny(missing_debug_implementations)]
#![deny(rust_2018_idioms)]
#![deny(unsafe_code)]

pub use la_arena;
use la_arena::{Arena, Idx};
use smol_str::SmolStr;

/// A name in code, aka a variable, an identifier.
#[derive(Debug)]
pub struct Name(SmolStr);

impl Name {
  /// Returns a new [`Name`].
  pub fn new<S>(s: S) -> Self
  where
    S: Into<SmolStr>,
  {
    Self(s.into())
  }
}

/// An expression.
#[derive(Debug)]
pub enum Expr {
  /// Nothing.
  None,
  /// Integer literal.
  Int(i32),
  /// Variable.
  Name(Name),
  /// Lambda, a function literal.
  Lam(Name, ExprIdx),
  /// Annotated lambda. The variable type is given by the [`Ty`].
  ALam(Name, TyIdx, ExprIdx),
  /// Function application.
  App(ExprIdx, ExprIdx),
  /// Variable binding. The variable's contents are the first [`Expr`] and it
  /// is is available in the scope of the latter [`Expr`].
  Let(Name, ExprIdx, ExprIdx),
  /// Annotation.
  Ann(ExprIdx, TyIdx),
}

pub type ExprIdx = Idx<Expr>;
pub type ExprArena = Arena<Expr>;

/// A type. "Sigma" in the MSR paper.
#[derive(Debug)]
pub enum Ty {
  /// Nothing.
  None,
  /// Forall types. The bound type variables may appear in the [`Ty`].
  ForAll(Vec<Name>, TyIdx),
  /// Functions, from arguments to results.
  Fun(TyIdx, TyIdx),
  /// Integer.
  Int,
  /// Variable.
  Name(Name),
}

pub type TyIdx = Idx<Ty>;
pub type TyArena = Arena<Ty>;
