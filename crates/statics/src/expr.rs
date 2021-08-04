//! Typechecking [`Expr`]s.

use crate::defs::{Cx, Env, Expr, Rho, Sigma, Ty, TyVar};
use crate::ty::{
  free_ty_vars, instantiate, meta_ty_vars, quantify, skolemize, unify,
  unify_fn, zonk,
};
use std::collections::HashSet;

/// Infer a type for `expr` under `env`.
pub fn infer_ty(cx: &mut Cx, env: &Env, expr: &Expr) -> Ty {
  let ty = infer_sigma(cx, env, expr);
  zonk(cx, ty)
}

/// A slot for a [`Rho`] that starts empty and may be set exactly once.
#[derive(Debug, Default)]
struct RhoRef(Option<Rho>);

impl RhoRef {
  fn set(&mut self, rho: Rho) {
    assert!(self.0.is_none());
    self.0 = Some(rho);
  }

  fn unwrap(self) -> Rho {
    self.0.unwrap()
  }
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
    Expr::Int(_) => inst_sigma(cx, Ty::Int, exp_ty),
    // @rule VAR
    Expr::Var(v) => {
      let ty = env.get(v).unwrap();
      inst_sigma(cx, ty.clone(), exp_ty)
    }
    Expr::Lam(var, body) => match exp_ty {
      // @rule ABS1
      Expected::Infer(exp_ty) => {
        let var_ty = Ty::MetaTyVar(cx.new_meta_ty_var());
        let env = env.clone().insert(*var, var_ty.clone());
        let body_ty = infer_rho(cx, &env, body);
        exp_ty.set(Ty::fun(var_ty, body_ty));
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
        exp_ty.set(Ty::fun(var_ty.clone(), body_ty));
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
      check_sigma(cx, env, arg, arg_ty);
      inst_sigma(cx, res_ty, exp_ty);
    }
    // @rule LET
    Expr::Let(var, rhs, body) => {
      let var_ty = infer_sigma(cx, env, rhs);
      let env = env.clone().insert(*var, var_ty);
      tc_rho(cx, &env, exp_ty, body);
    }
    // @rule ANNOT
    Expr::Ann(body, ann_ty) => {
      check_sigma(cx, env, body, ann_ty.clone());
      inst_sigma(cx, ann_ty.clone(), exp_ty);
    }
  }
}

fn infer_sigma(cx: &mut Cx, env: &Env, expr: &Expr) -> Sigma {
  // @rule GEN1
  let exp_ty = infer_rho(cx, env, expr);
  let mut env_tvs = HashSet::new();
  for sigma in env.values() {
    meta_ty_vars(&mut env_tvs, sigma);
  }
  let mut res_tvs = HashSet::new();
  meta_ty_vars(&mut res_tvs, &exp_ty);
  for tv in env_tvs {
    res_tvs.remove(&tv);
  }
  quantify(cx, &res_tvs, exp_ty)
}

fn check_sigma(cx: &mut Cx, env: &Env, expr: &Expr, sigma: Sigma) {
  // @rule GEN2
  let mut skol_tvs = Vec::new();
  let rho = skolemize(cx, &mut skol_tvs, sigma.clone());
  check_rho(cx, env, expr, rho);
  let mut env_tvs = HashSet::new();
  free_ty_vars(&mut env_tvs, &zonk(cx, sigma));
  for sigma in env.values() {
    free_ty_vars(&mut env_tvs, &zonk(cx, sigma.clone()));
  }
  for skol_tv in skol_tvs {
    if env_tvs.contains(&TyVar::Skolem(skol_tv)) {
      panic!("type not polymorphic enough")
    }
  }
}

fn subs_check(cx: &mut Cx, sigma1: Sigma, sigma2: Sigma) {
  // @rule DEEP-SKOL
  let mut skol_tvs = Vec::new();
  let rho2 = skolemize(cx, &mut skol_tvs, sigma2.clone());
  subs_check_rho(cx, sigma1.clone(), rho2);
  let mut enc_tvs = HashSet::new();
  free_ty_vars(&mut enc_tvs, &zonk(cx, sigma1));
  free_ty_vars(&mut enc_tvs, &zonk(cx, sigma2));
  if skol_tvs
    .into_iter()
    .any(|tv| enc_tvs.contains(&TyVar::Skolem(tv)))
  {
    panic!("sigma1 not as polymorphic as sigma2")
  }
}

fn subs_check_rho(cx: &mut Cx, sigma: Sigma, rho: Rho) {
  match (sigma, rho) {
    // @rule SPEC
    (sigma @ Ty::ForAll(_, _), rho) => {
      let rho1 = instantiate(cx, sigma);
      subs_check_rho(cx, rho1, rho)
    }
    // @rule FUN
    (rho1, Ty::Fun(arg_ty2, res_ty2)) => {
      let (arg_ty1, res_ty1) = unify_fn(cx, &rho1);
      subs_check_fun(cx, arg_ty1, res_ty1, *arg_ty2, *res_ty2);
    }
    // this one too
    (Ty::Fun(arg_ty1, res_ty1), rho2) => {
      let (arg_ty2, res_ty2) = unify_fn(cx, &rho2);
      subs_check_fun(cx, *arg_ty1, *res_ty1, arg_ty2, res_ty2);
    }
    // @rule MONO
    (rho1, rho2) => unify(cx, &rho1, &rho2),
  }
}

fn subs_check_fun(
  cx: &mut Cx,
  arg_ty1: Sigma,
  res_ty1: Rho,
  arg_ty2: Sigma,
  res_ty2: Rho,
) {
  subs_check(cx, arg_ty2, arg_ty1);
  subs_check(cx, res_ty1, res_ty2);
}

fn inst_sigma(cx: &mut Cx, ty1: Sigma, exp_ty: Expected<'_>) {
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
