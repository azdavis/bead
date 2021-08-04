//! Typechecking [`Expr`]s.

use crate::ty::{
  free_ty_vars, instantiate, meta_ty_vars, quantify, skolemize, unify,
  unify_fn, zonk,
};
use defs::{Cx, Env, Expr, Rho, RhoRef, Ty, TyVar};
use std::collections::HashSet;

/// Infer a type for `expr` under `env`.
pub fn infer_ty_zonk(cx: &mut Cx, env: &Env, expr: &Expr) -> Ty {
  let ty = infer_ty(cx, env, expr);
  zonk(cx, ty)
}

/// The direction of typechecking.
enum Expected<'a> {
  /// Extract a type *up* from an expression and store it in the [`RhoRef`].
  Infer(&'a mut RhoRef),
  /// Push a type *down* into an expression.
  Check(Rho),
}

fn infer_rho(cx: &mut Cx, env: &Env, expr: &Expr) -> Rho {
  let mut ret = RhoRef::default();
  tc_rho(cx, env, Expected::Infer(&mut ret), expr);
  ret.unwrap()
}

fn check_rho(cx: &mut Cx, env: &Env, expr: &Expr, rho: Rho) {
  tc_rho(cx, env, Expected::Check(rho), expr)
}

fn tc_rho(cx: &mut Cx, env: &Env, exp_ty: Expected<'_>, expr: &Expr) {
  match expr {
    // @rule INT
    Expr::Int(_) => inst_ty(cx, Ty::Int, exp_ty),
    // @rule VAR
    Expr::Var(v) => {
      let ty = env.get(v).unwrap();
      inst_ty(cx, ty.clone(), exp_ty)
    }
    Expr::Lam(var, body) => match exp_ty {
      // @rule ABS1
      Expected::Infer(exp_ty) => {
        let var_ty = Ty::MetaTyVar(cx.new_meta_ty_var());
        let env = env.clone().insert(*var, var_ty.clone());
        let body_ty = infer_rho(cx, &env, body);
        exp_ty.set(Rho::new(Ty::fun(var_ty, body_ty.into_inner())));
      }
      // @rule ABS2
      Expected::Check(exp_ty) => {
        let (var_ty, body_ty) = unify_fn(cx, &exp_ty);
        let env = env.clone().insert(*var, var_ty);
        check_rho(cx, &env, body, body_ty);
      }
    },
    Expr::ALam(var, var_ty, body) => match exp_ty {
      // @rule AABS1
      Expected::Infer(exp_ty) => {
        let env = env.clone().insert(*var, var_ty.clone());
        let body_ty = infer_rho(cx, &env, body);
        exp_ty.set(Rho::new(Ty::fun(var_ty.clone(), body_ty.into_inner())));
      }
      // @rule AABS2
      Expected::Check(exp_ty) => {
        let (arg_ty, body_ty) = unify_fn(cx, &exp_ty);
        subs_check(cx, arg_ty, var_ty.clone());
        let env = env.clone().insert(*var, var_ty.clone());
        check_rho(cx, &env, body, body_ty);
      }
    },
    // @rule APP
    Expr::App(fun, arg) => {
      let fun_ty = infer_rho(cx, env, fun);
      let (arg_ty, res_ty) = unify_fn(cx, &fun_ty);
      check_ty(cx, env, arg, arg_ty);
      inst_ty(cx, res_ty.into_inner(), exp_ty);
    }
    // @rule LET
    Expr::Let(var, rhs, body) => {
      let var_ty = infer_ty(cx, env, rhs);
      let env = env.clone().insert(*var, var_ty);
      tc_rho(cx, &env, exp_ty, body);
    }
    // @rule ANNOT
    Expr::Ann(body, ann_ty) => {
      check_ty(cx, env, body, ann_ty.clone());
      inst_ty(cx, ann_ty.clone(), exp_ty);
    }
  }
}

fn infer_ty(cx: &mut Cx, env: &Env, expr: &Expr) -> Ty {
  // @rule GEN1
  let exp_ty = infer_rho(cx, env, expr);
  let mut env_tvs = HashSet::new();
  for ty in env.values() {
    meta_ty_vars(&mut env_tvs, ty);
  }
  let mut res_tvs = HashSet::new();
  meta_ty_vars(&mut res_tvs, exp_ty.as_ref());
  for tv in env_tvs {
    res_tvs.remove(&tv);
  }
  quantify(cx, &res_tvs, exp_ty)
}

fn check_ty(cx: &mut Cx, env: &Env, expr: &Expr, ty: Ty) {
  // @rule GEN2
  let mut skol_tvs = Vec::new();
  let rho = skolemize(cx, &mut skol_tvs, ty.clone());
  check_rho(cx, env, expr, rho);
  let mut env_tvs = HashSet::new();
  free_ty_vars(cx, &mut env_tvs, &ty);
  for ty in env.values() {
    free_ty_vars(cx, &mut env_tvs, ty);
  }
  for skol_tv in skol_tvs {
    if env_tvs.contains(&TyVar::Skolem(skol_tv)) {
      panic!("type not polymorphic enough")
    }
  }
}

fn subs_check(cx: &mut Cx, ty1: Ty, ty2: Ty) {
  // @rule DEEP-SKOL
  let mut skol_tvs = Vec::new();
  let rho2 = skolemize(cx, &mut skol_tvs, ty2.clone());
  subs_check_rho(cx, ty1.clone(), rho2);
  let mut enc_tvs = HashSet::new();
  free_ty_vars(cx, &mut enc_tvs, &ty1);
  free_ty_vars(cx, &mut enc_tvs, &ty2);
  if skol_tvs
    .into_iter()
    .any(|tv| enc_tvs.contains(&TyVar::Skolem(tv)))
  {
    panic!("ty1 not as polymorphic as ty2")
  }
}

fn subs_check_rho(cx: &mut Cx, ty: Ty, rho: Rho) {
  match (ty, rho.into_inner()) {
    // @rule SPEC
    (ty @ Ty::ForAll(_, _), rho) => {
      let rho1 = instantiate(cx, ty);
      subs_check_rho(cx, rho1.into_inner(), Rho::new(rho))
    }
    // @rule FUN
    (rho1, Ty::Fun(arg_ty2, res_ty2)) => {
      let (arg_ty1, res_ty1) = unify_fn(cx, &Rho::new(rho1));
      subs_check_fun(cx, arg_ty1, res_ty1, *arg_ty2, Rho::new(*res_ty2));
    }
    // this one too
    (Ty::Fun(arg_ty1, res_ty1), rho2) => {
      let (arg_ty2, res_ty2) = unify_fn(cx, &Rho::new(rho2));
      subs_check_fun(cx, *arg_ty1, Rho::new(*res_ty1), arg_ty2, res_ty2);
    }
    // @rule MONO
    (rho1, rho2) => unify(cx, &rho1, &rho2),
  }
}

fn subs_check_fun(
  cx: &mut Cx,
  arg_ty1: Ty,
  res_ty1: Rho,
  arg_ty2: Ty,
  res_ty2: Rho,
) {
  subs_check(cx, arg_ty2, arg_ty1);
  subs_check(cx, res_ty1.into_inner(), res_ty2.into_inner());
}

fn inst_ty(cx: &mut Cx, ty1: Ty, exp_ty: Expected<'_>) {
  match exp_ty {
    // @rule INST1
    Expected::Infer(r) => {
      let ty = instantiate(cx, ty1);
      r.set(ty);
    }
    // @rule INST2
    Expected::Check(ty2) => subs_check_rho(cx, ty1, ty2),
  }
}
