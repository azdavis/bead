//! Typechecking [`Expr`]s.

use crate::defs::Env;
use crate::defs::{Cx, Entity as E, ErrorKind as EK, Rho, RhoRef, Ty, TyVar};
use crate::lower;
use crate::ty::{
  free_ty_vars, instantiate, meta_ty_vars, quantify, skolemize, unify, unify_fn,
};
use hir::{Arenas, Expr, ExprIdx};
use rustc_hash::FxHashSet;

/// The direction of typechecking.
#[derive(Debug)]
enum Mode<'a> {
  /// Extract a type *up* from an expression and store it in the [`RhoRef`].
  Infer(&'a mut RhoRef),
  /// Push a type *down* into an expression.
  Check(Rho),
}

/// infers (synthesizes) a rho type for `expr` under `env`.
fn infer_rho(cx: &mut Cx, arenas: &Arenas, env: &Env, expr: ExprIdx) -> Rho {
  let mut r = RhoRef::default();
  tc_rho(cx, arenas, env, Mode::Infer(&mut r), expr);
  r.expect("the RhoRef should be set")
}

/// checks `expr` has the `rho` type under `env`.
fn check_rho(cx: &mut Cx, arenas: &Arenas, env: &Env, expr: ExprIdx, rho: Rho) {
  tc_rho(cx, arenas, env, Mode::Check(rho), expr)
}

/// the main typechecking algorithm.
fn tc_rho(
  cx: &mut Cx,
  arenas: &Arenas,
  env: &Env,
  mode: Mode<'_>,
  expr: ExprIdx,
) {
  match arenas.expr[expr] {
    Expr::None => match mode {
      Mode::Infer(r) => r.set(Rho::new(Ty::None)),
      Mode::Check(_) => {}
    },
    // @rule INT
    Expr::Int(_) => inst_ty(cx, E::Expr(expr), Ty::Int, mode),
    // no rule, but similar to INT
    Expr::Str(_) => inst_ty(cx, E::Expr(expr), Ty::Str, mode),
    // @rule VAR
    Expr::Name(ref name) => {
      let ty = env.get(name).cloned().unwrap_or_else(|| {
        cx.err(E::Expr(expr), EK::NotInScope(name.clone()));
        Ty::None
      });
      inst_ty(cx, E::Expr(expr), ty, mode)
    }
    Expr::Lam(ref var, body) => match mode {
      // @rule ABS1
      Mode::Infer(r) => {
        let var_ty = Ty::MetaTyVar(cx.new_meta_ty_var());
        let env = env.clone().insert(var.clone(), var_ty.clone());
        let body_ty = infer_rho(cx, arenas, &env, body);
        r.set(Rho::new(Ty::fun(var_ty, body_ty.into_inner())));
      }
      // @rule ABS2
      Mode::Check(rho) => {
        let (var_ty, body_ty) = unify_fn(cx, E::Expr(expr), &rho);
        let env = env.clone().insert(var.clone(), var_ty);
        check_rho(cx, arenas, &env, body, body_ty);
      }
    },
    Expr::ALam(ref var, var_ty, body) => match mode {
      // @rule AABS1
      Mode::Infer(r) => {
        let var_ty = lower::ty(cx, arenas, var_ty);
        let env = env.clone().insert(var.clone(), var_ty.clone());
        let body_ty = infer_rho(cx, arenas, &env, body);
        r.set(Rho::new(Ty::fun(var_ty, body_ty.into_inner())));
      }
      // @rule AABS2
      Mode::Check(rho) => {
        let var_ty = lower::ty(cx, arenas, var_ty);
        let (arg_ty, body_ty) = unify_fn(cx, E::Expr(expr), &rho);
        subs_check(cx, E::Expr(expr), arg_ty, var_ty.clone());
        let env = env.clone().insert(var.clone(), var_ty);
        check_rho(cx, arenas, &env, body, body_ty);
      }
    },
    // @rule APP
    Expr::App(fun, arg) => {
      let fun_ty = infer_rho(cx, arenas, env, fun);
      let (arg_ty, res_ty) = unify_fn(cx, E::Expr(expr), &fun_ty);
      check_ty(cx, arenas, env, arg, arg_ty);
      inst_ty(cx, E::Expr(expr), res_ty.into_inner(), mode);
    }
    // @rule LET
    Expr::Let(ref var, rhs, body) => {
      let var_ty = infer_ty(cx, arenas, env, rhs);
      let env = env.clone().insert(var.clone(), var_ty);
      tc_rho(cx, arenas, &env, mode, body);
    }
    // @rule ANNOT
    Expr::Ann(body, ann_ty) => {
      let ann_ty = lower::ty(cx, arenas, ann_ty);
      check_ty(cx, arenas, env, body, ann_ty.clone());
      inst_ty(cx, E::Expr(expr), ann_ty, mode);
    }
  }
}

/// infer a rho type for `expr`, then generalize it to a forall.
///
/// NOTE: in the paper, the top-level algorithm first does this, then does
/// subst. but this always returns what `quantify` returns, and quantify already
/// does the subst. so the paper seems to be doing an extra unnecessary subst.
pub(crate) fn infer_ty(
  cx: &mut Cx,
  arenas: &Arenas,
  env: &Env,
  expr: ExprIdx,
) -> Ty {
  // @rule GEN1
  let mode = infer_rho(cx, arenas, env, expr);
  let mut env_tvs = FxHashSet::default();
  for ty in env.values() {
    meta_ty_vars(cx, &mut env_tvs, ty);
  }
  let mut res_tvs = FxHashSet::default();
  meta_ty_vars(cx, &mut res_tvs, mode.as_ref());
  for tv in env_tvs {
    res_tvs.remove(&tv);
  }
  quantify(cx, &res_tvs, mode)
}

/// check the `expr` has the `ty` under `env`.
fn check_ty(cx: &mut Cx, arenas: &Arenas, env: &Env, expr: ExprIdx, ty: Ty) {
  // @rule GEN2
  let mut skol_tvs = Vec::new();
  let rho = skolemize(cx, &mut skol_tvs, ty.clone());
  check_rho(cx, arenas, env, expr, rho);
  let mut env_tvs = FxHashSet::default();
  free_ty_vars(cx, &mut env_tvs, &ty);
  for ty in env.values() {
    free_ty_vars(cx, &mut env_tvs, ty);
  }
  if skol_tvs
    .into_iter()
    .any(|skol_tv| env_tvs.contains(&TyVar::Skolem(skol_tv)))
  {
    cx.err(E::Expr(expr), EK::NotPolymorphicEnough(ty));
  }
}

/// checks that `ty1` is at least as polymorphic as `ty2`.
fn subs_check(cx: &mut Cx, entity: E, ty1: Ty, ty2: Ty) {
  // @rule DEEP-SKOL
  let mut skol_tvs = Vec::new();
  let rho2 = skolemize(cx, &mut skol_tvs, ty2.clone());
  subs_check_rho(cx, entity, ty1.clone(), rho2);
  let mut enc_tvs = FxHashSet::default();
  free_ty_vars(cx, &mut enc_tvs, &ty1);
  free_ty_vars(cx, &mut enc_tvs, &ty2);
  if skol_tvs
    .into_iter()
    .any(|tv| enc_tvs.contains(&TyVar::Skolem(tv)))
  {
    cx.err(entity, EK::NotAsPolymorphicAsOther(ty1, ty2));
  }
}

/// checks that `ty` is at least as polymorphic as `rho`.
fn subs_check_rho(cx: &mut Cx, entity: E, ty: Ty, rho: Rho) {
  match (ty, rho.into_inner()) {
    // @rule SPEC
    (ty @ Ty::ForAll(_, _), rho) => {
      let rho1 = instantiate(cx, ty);
      subs_check_rho(cx, entity, rho1.into_inner(), Rho::new(rho))
    }
    // @rule FUN
    (rho1, Ty::Fun(a2, r2)) => {
      let (a1, r1) = unify_fn(cx, entity, &Rho::new(rho1));
      subs_check_fun(cx, entity, a1, r1, *a2, Rho::new(*r2));
    }
    // this one too
    (Ty::Fun(a1, r1), rho2) => {
      let (a2, r2) = unify_fn(cx, entity, &Rho::new(rho2));
      subs_check_fun(cx, entity, *a1, Rho::new(*r1), a2, r2);
    }
    // @rule MONO
    (rho1, rho2) => unify(cx, entity, &rho1, &rho2),
  }
}

/// check that `arg_ty1 -> res_ty1` is at least as polymorphic as `arg_ty2 ->
/// res_ty2`.
fn subs_check_fun(cx: &mut Cx, entity: E, a1: Ty, r1: Rho, a2: Ty, r2: Rho) {
  subs_check(cx, entity, a2, a1);
  subs_check(cx, entity, r1.into_inner(), r2.into_inner());
}

fn inst_ty(cx: &mut Cx, entity: E, ty: Ty, mode: Mode<'_>) {
  match mode {
    // @rule INST1
    Mode::Infer(r) => {
      let ty = instantiate(cx, ty);
      r.set(ty);
    }
    // @rule INST2
    Mode::Check(rho) => subs_check_rho(cx, entity, ty, rho),
  }
}
