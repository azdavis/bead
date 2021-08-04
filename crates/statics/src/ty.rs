//! Operations on [`Ty`]s.

use crate::defs::{
  BoundTyVar, Cx, MetaTyVar, Name, Rho, Sigma, SkolemTyVar, Ty, TyVar,
};
use std::collections::{HashMap, HashSet};

pub(crate) fn meta_ty_vars(ac: &mut HashSet<MetaTyVar>, ty: &Ty) {
  match ty {
    Ty::ForAll(_, ty) => meta_ty_vars(ac, ty),
    Ty::Fun(arg_ty, res_ty) => {
      meta_ty_vars(ac, arg_ty);
      meta_ty_vars(ac, res_ty);
    }
    // the only interesting case
    Ty::MetaTyVar(tv) => {
      ac.insert(*tv);
    }
    Ty::Int | Ty::TyVar(_) => {}
  }
}

pub(crate) fn free_ty_vars(ac: &mut HashSet<TyVar>, ty: &Ty) {
  match ty {
    // `tvs` are bound, *not* free
    Ty::ForAll(tvs, ty) => {
      free_ty_vars(ac, ty);
      for &tv in tvs {
        ac.remove(&TyVar::Bound(tv));
      }
    }
    Ty::Fun(arg_ty, res_ty) => {
      free_ty_vars(ac, arg_ty);
      free_ty_vars(ac, res_ty);
    }
    // might be free, will be removed if bound
    Ty::TyVar(tv) => {
      ac.insert(*tv);
    }
    Ty::Int | Ty::MetaTyVar(_) => {}
  }
}

fn bound_ty_vars(ac: &mut HashSet<BoundTyVar>, ty: &Ty) {
  match ty {
    // `tvs` are bound
    Ty::ForAll(tvs, ty) => {
      ac.extend(tvs.iter().copied());
      bound_ty_vars(ac, ty);
    }
    Ty::Fun(arg_ty, res_ty) => {
      bound_ty_vars(ac, arg_ty);
      bound_ty_vars(ac, res_ty);
    }
    Ty::Int | Ty::TyVar(_) | Ty::MetaTyVar(_) => {}
  }
}

/// this must not induce capture.
fn subst(map: &HashMap<BoundTyVar, Ty>, ty: Ty) -> Ty {
  match ty {
    Ty::ForAll(tvs, ty) => {
      let mut map = map.clone();
      for tv in tvs {
        map.remove(&tv);
      }
      subst(&map, *ty)
    }
    Ty::Fun(arg_ty, res_ty) => {
      let arg_ty = subst(map, *arg_ty);
      let res_ty = subst(map, *res_ty);
      Ty::fun(arg_ty, res_ty)
    }
    Ty::Int => Ty::Int,
    Ty::TyVar(tv) => match tv {
      TyVar::Bound(tv) => match map.get(&tv) {
        None => Ty::TyVar(TyVar::Bound(tv)),
        Some(ty) => ty.clone(),
      },
      TyVar::Skolem(tv) => Ty::TyVar(TyVar::Skolem(tv)),
    },
    Ty::MetaTyVar(tv) => Ty::MetaTyVar(tv),
  }
}

/// removes top-level forall, and replaces all type variables previously bound
/// by those forall with new meta type variables.
pub(crate) fn instantiate(cx: &mut Cx, ty: Sigma) -> Rho {
  match ty {
    Ty::ForAll(tvs, ty) => {
      let map: HashMap<_, _> = tvs
        .into_iter()
        .map(|tv| (tv, Ty::MetaTyVar(cx.new_meta_ty_var())))
        .collect();
      // no capture because MetaTyVars cannot be bound and they are all new.
      let ty = subst(&map, *ty);
      // NOTE: this handles nested ForAlls, but the haskell code does not.
      instantiate(cx, ty)
    }
    _ => ty,
  }
}

/// removes forall not directly to the left of arrows, and replaces type
/// variables previously bound by those forall with new skolem type variables.
pub(crate) fn skolemize(
  cx: &mut Cx,
  ac: &mut Vec<SkolemTyVar>,
  ty: Sigma,
) -> Rho {
  match ty {
    // @rule PRPOLY
    Ty::ForAll(tvs, ty) => {
      let map: HashMap<_, _> = tvs
        .into_iter()
        .map(|tv| {
          let sk = cx.new_skolem_ty_var(TyVar::Bound(tv));
          ac.push(sk);
          (tv, Ty::TyVar(TyVar::Skolem(sk)))
        })
        .collect();
      // no capture because skolem ty vars cannot be bound and they are all new.
      let ty = subst(&map, *ty);
      skolemize(cx, ac, ty)
    }
    // @rule PRFUN
    Ty::Fun(ty1, ty2) => {
      let ty2 = skolemize(cx, ac, *ty2);
      Ty::Fun(ty1, Box::new(ty2))
    }
    // @rule PRMONO
    _ => ty,
  }
}

/// replaces all meta type variables with new bound type variables, and binds
/// them with a top-level forall.
pub(crate) fn quantify(
  cx: &mut Cx,
  set: &HashSet<MetaTyVar>,
  ty: Rho,
) -> Sigma {
  let mut used_bound = HashSet::new();
  bound_ty_vars(&mut used_bound, &ty);
  let mut i = 0u32;
  let mut iter = set.iter();
  let mut new_bound = Vec::with_capacity(set.len());
  while iter.len() != 0 {
    let bound_tv = BoundTyVar::new(Name::new(i));
    i += 1;
    if used_bound.contains(&bound_tv) {
      continue;
    }
    new_bound.push(bound_tv);
    let meta_tv = *iter.next().expect("checked len != 0");
    cx.set(meta_tv, Ty::TyVar(TyVar::Bound(bound_tv)));
  }
  Ty::ForAll(new_bound, Box::new(zonk(cx, ty)))
}

/// replaces all meta type variables with their corresponding types from the
/// context.
pub(crate) fn zonk(cx: &mut Cx, ty: Ty) -> Ty {
  match ty {
    Ty::ForAll(tvs, ty) => {
      let ty = Box::new(zonk(cx, *ty));
      Ty::ForAll(tvs, ty)
    }
    Ty::Fun(arg_ty, res_ty) => {
      let arg_ty = zonk(cx, *arg_ty);
      let res_ty = zonk(cx, *res_ty);
      Ty::fun(arg_ty, res_ty)
    }
    Ty::Int => Ty::Int,
    Ty::TyVar(tv) => Ty::TyVar(tv),
    // the only interesting case
    Ty::MetaTyVar(tv) => match cx.get(tv) {
      None => Ty::MetaTyVar(tv),
      Some(ty) => {
        let ty = ty.clone();
        let ty = zonk(cx, ty);
        cx.set(tv, ty.clone());
        ty
      }
    },
  }
}

pub(crate) fn unify(cx: &mut Cx, ty1: &Ty, ty2: &Ty) {
  match (ty1, ty2) {
    (Ty::TyVar(TyVar::Bound(_)), _) | (_, Ty::TyVar(TyVar::Bound(_))) => {
      unreachable!("bound ty vars in unify")
    }
    (Ty::ForAll(..), _) | (_, Ty::ForAll(..)) => {
      // NOTE: this explicitly forbids forall, but the haskell code does not.
      unreachable!("forall in unify")
    }
    (Ty::TyVar(tv1), Ty::TyVar(tv2)) => assert!(tv1 == tv2),
    (Ty::MetaTyVar(tv1), Ty::MetaTyVar(tv2)) => assert!(tv1 == tv2),
    (Ty::MetaTyVar(tv1), ty2) | (ty2, Ty::MetaTyVar(tv1)) => {
      if let Some(ty1) = cx.get(*tv1).cloned() {
        return unify(cx, &ty1, ty2);
      }
      match ty2 {
        Ty::MetaTyVar(tv2) => match cx.get(*tv2).cloned() {
          None => cx.set(*tv1, ty2.clone()),
          Some(ty2) => unify(cx, &Ty::MetaTyVar(*tv1), &ty2),
        },
        _ => {
          let mut meta_tvs = HashSet::new();
          meta_ty_vars(&mut meta_tvs, ty2);
          if meta_tvs.contains(tv1) {
            panic!("occurs check failed")
          }
          cx.set(*tv1, ty2.clone());
        }
      }
    }
    (Ty::Int, Ty::Int) => {}
    (Ty::Fun(arg_ty1, res_ty1), Ty::Fun(arg_ty2, res_ty2)) => {
      unify(cx, arg_ty1, arg_ty2);
      unify(cx, res_ty1, res_ty2);
    }
    (Ty::Int, _) | (_, Ty::Int) | (Ty::Fun(..), _) | (_, Ty::Fun(..)) => {
      panic!("cannot unify")
    }
  }
}

pub(crate) fn unify_fn(cx: &mut Cx, ty: &Rho) -> (Sigma, Rho) {
  match ty {
    Ty::Fun(arg_ty, res_ty) => ((**arg_ty).clone(), (**res_ty).clone()),
    _ => {
      let arg_ty = Ty::MetaTyVar(cx.new_meta_ty_var());
      let res_ty = Ty::MetaTyVar(cx.new_meta_ty_var());
      let fn_ty = Ty::fun(arg_ty.clone(), res_ty.clone());
      unify(cx, &fn_ty, ty);
      (arg_ty, res_ty)
    }
  }
}