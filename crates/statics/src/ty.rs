//! Operations on [`Ty`]s.

use crate::defs::{
  BoundTyVar, Cx, Entity as E, ErrorKind as EK, MetaTyVar, Rho, SkolemTyVar,
  Tau, Ty, TyVar,
};
use hir::Name;
use rustc_hash::{FxHashMap, FxHashSet};

/// this substitutes on the fly.
///
/// because substitution replaces bound meta type variables with the types they
/// have been bound to, this has the effect of only adding unbound meta type
/// variables to `ac`.
pub(crate) fn meta_ty_vars(cx: &Cx, ac: &mut FxHashSet<MetaTyVar>, ty: &Ty) {
  match ty {
    // the most interesting case. see the case in `subst`.
    Ty::MetaTyVar(tv) => match cx.get(*tv) {
      None => {
        ac.insert(*tv);
      }
      Some(ty) => meta_ty_vars(cx, ac, ty.as_ref()),
    },
    Ty::ForAll(_, ty) => meta_ty_vars(cx, ac, (**ty).as_ref()),
    Ty::Fun(arg_ty, res_ty) => {
      meta_ty_vars(cx, ac, arg_ty);
      meta_ty_vars(cx, ac, res_ty);
    }
    Ty::None | Ty::Int | Ty::Str | Ty::TyVar(_) => {}
  }
}

/// this does substitution on the fly.
pub(crate) fn free_ty_vars(cx: &Cx, ac: &mut FxHashSet<TyVar>, ty: &Ty) {
  match ty {
    // `tvs` are bound, *not* free
    Ty::ForAll(tvs, ty) => {
      free_ty_vars(cx, ac, (**ty).as_ref());
      for tv in tvs {
        ac.remove(&TyVar::Bound(tv.clone()));
      }
    }
    // might be free, will be removed if bound
    Ty::TyVar(tv) => {
      ac.insert(tv.clone());
    }
    // see the case in `subst`.
    Ty::MetaTyVar(tv) => match cx.get(*tv) {
      None => {}
      Some(ty) => free_ty_vars(cx, ac, ty.as_ref()),
    },
    Ty::Fun(arg_ty, res_ty) => {
      free_ty_vars(cx, ac, arg_ty);
      free_ty_vars(cx, ac, res_ty);
    }
    Ty::None | Ty::Int | Ty::Str => {}
  }
}

/// this does _not_ subst, nor does it need to. this is because [`MetaTyVar`]s
/// can only be bound to monotypes, which by definition contain no
/// [`Ty::ForAll`]. since, therefore, substitution will never change what
/// variables we add to `ac`, we don't bother to do it.
fn bound_ty_vars(ac: &mut FxHashSet<BoundTyVar>, ty: &Ty) {
  match ty {
    // `tvs` are bound
    Ty::ForAll(tvs, ty) => {
      ac.extend(tvs.iter().cloned());
      bound_ty_vars(ac, (**ty).as_ref());
    }
    Ty::Fun(arg_ty, res_ty) => {
      bound_ty_vars(ac, arg_ty);
      bound_ty_vars(ac, res_ty);
    }
    Ty::None | Ty::Int | Ty::Str | Ty::TyVar(_) | Ty::MetaTyVar(_) => {}
  }
}

/// replaces the bound ty vars with their corresponding ty in the given ty.
/// takes into account the fact that forall inside the ty prevent substitution
/// because they are binders.
///
/// the caller must arrange so that this does not induce capture.
fn subst_bound_tv(map: &FxHashMap<BoundTyVar, Ty>, ty: &mut Ty) {
  match ty {
    // the most interesting case, which does the subst
    Ty::TyVar(tv) => match tv {
      TyVar::Bound(tv) => match map.get(tv) {
        None => {}
        Some(got_ty) => *ty = got_ty.clone(),
      },
      TyVar::Skolem(_) => {}
    },
    // the forall binds some vars
    Ty::ForAll(tvs, ty) => {
      let mut map = map.clone();
      for tv in tvs.iter() {
        map.remove(tv);
      }
      ty.use_mut(|ty| subst_bound_tv(&map, ty));
    }
    Ty::Fun(arg_ty, res_ty) => {
      subst_bound_tv(map, arg_ty);
      subst_bound_tv(map, res_ty);
    }
    Ty::None | Ty::Int | Ty::Str | Ty::MetaTyVar(_) => {}
  }
}

/// removes top-level forall, and replaces all type variables previously bound
/// by those forall with new meta type variables.
pub(crate) fn instantiate(cx: &mut Cx, ty: Ty) -> Rho {
  match ty {
    Ty::ForAll(tvs, mut ty) => {
      let map: FxHashMap<_, _> = tvs
        .into_iter()
        .map(|tv| (tv, Ty::MetaTyVar(cx.new_meta_ty_var())))
        .collect();
      // no capture because MetaTyVars cannot be bound and they are all new.
      ty.use_mut(|ty| subst_bound_tv(&map, ty));
      *ty
    }
    _ => Rho::new(ty),
  }
}

/// removes forall not directly to the left of arrows, and replaces type
/// variables previously bound by those forall with new skolem type variables.
pub(crate) fn skolemize(cx: &mut Cx, ac: &mut Vec<SkolemTyVar>, ty: Ty) -> Rho {
  match ty {
    // @rule PRPOLY
    Ty::ForAll(tvs, ty) => {
      let map: FxHashMap<_, _> = tvs
        .into_iter()
        .map(|tv| {
          let sk = cx.new_skolem_ty_var(TyVar::Bound(tv.clone()));
          ac.push(sk);
          (tv, Ty::TyVar(TyVar::Skolem(sk)))
        })
        .collect();
      // no capture because skolem ty vars cannot be bound and they are all new.
      let mut ty = ty.into_inner();
      subst_bound_tv(&map, &mut ty);
      skolemize(cx, ac, ty)
    }
    // @rule PRFUN
    Ty::Fun(ty1, ty2) => {
      let ty2 = skolemize(cx, ac, *ty2);
      Rho::new(Ty::Fun(ty1, Box::new(ty2.into_inner())))
    }
    // @rule PRMONO
    _ => Rho::new(ty),
  }
}

/// replaces the meta type variables in `set` with new bound type variables, and
/// binds them with a top-level forall. this returns a type already `subst`'d.
pub(crate) fn quantify(
  cx: &mut Cx,
  set: &FxHashSet<MetaTyVar>,
  mut ty: Rho,
) -> Ty {
  let mut used_bound = FxHashSet::default();
  bound_ty_vars(&mut used_bound, ty.as_ref());
  let mut iter = set.iter();
  let mut new_bound = Vec::with_capacity(set.len());
  let mut n = 0u32;
  while iter.len() != 0 {
    let bound_tv = BoundTyVar::new(Name::new(format!("t{}", n)));
    n += 1;
    if used_bound.contains(&bound_tv) {
      continue;
    }
    new_bound.push(bound_tv.clone());
    let meta_tv = *iter.next().expect("checked len != 0");
    cx.set(meta_tv, Tau::new(Ty::TyVar(TyVar::Bound(bound_tv))));
  }
  ty.use_mut(|ty| subst(cx, ty));
  Ty::for_all(new_bound, ty)
}

/// replaces all meta type variables with their corresponding types from the
/// context.
fn subst(cx: &mut Cx, ty: &mut Ty) {
  match ty {
    // the most interesting case
    Ty::MetaTyVar(tv) => match cx.get(*tv) {
      None => {}
      Some(got_ty) => {
        let mut got_ty = got_ty.clone().into_inner();
        subst(cx, &mut got_ty);
        cx.reset(*tv, Tau::new(got_ty.clone()));
        *ty = got_ty;
      }
    },
    Ty::ForAll(_, ty) => ty.use_mut(|ty| subst(cx, ty)),
    Ty::Fun(arg_ty, res_ty) => {
      subst(cx, arg_ty);
      subst(cx, res_ty);
    }
    Ty::None | Ty::Int | Ty::Str | Ty::TyVar(_) => {}
  }
}

pub(crate) fn unify(cx: &mut Cx, entity: E, ty1: Ty, ty2: Ty) {
  match (ty1, ty2) {
    // NOTE: this explicitly forbids forall, but the haskell code does not.
    (Ty::TyVar(TyVar::Bound(_)), _)
    | (_, Ty::TyVar(TyVar::Bound(_)))
    | (Ty::ForAll(_, _), _)
    | (_, Ty::ForAll(_, _)) => {
      unreachable!("bad types in unify")
    }
    (Ty::Int, Ty::Int) | (Ty::Str, Ty::Str) | (Ty::None, _) | (_, Ty::None) => {
      // nothing
    }
    (Ty::TyVar(tv1), Ty::TyVar(tv2)) => {
      if tv1 != tv2 {
        cx.err(entity, EK::CannotUnify(Ty::TyVar(tv1), Ty::TyVar(tv2)));
      }
    }
    (Ty::MetaTyVar(tv1), ty2) | (ty2, Ty::MetaTyVar(tv1)) => {
      if let Ty::MetaTyVar(tv2) = ty2 {
        if tv1 == tv2 {
          return;
        }
      }
      if let Some(ty1) = cx.get(tv1).cloned() {
        return unify(cx, entity, ty1.into_inner(), ty2);
      }
      match ty2 {
        Ty::MetaTyVar(tv2) => match cx.get(tv2).cloned() {
          None => cx.set(tv1, Tau::new(ty2)),
          Some(ty2) => unify(cx, entity, Ty::MetaTyVar(tv1), ty2.into_inner()),
        },
        _ => {
          let mut meta_tvs = FxHashSet::default();
          meta_ty_vars(cx, &mut meta_tvs, &ty2);
          let t = if meta_tvs.contains(&tv1) {
            cx.err(entity, EK::OccursCheckFailed(ty2.clone(), tv1));
            Ty::None
          } else {
            ty2.clone()
          };
          cx.set(tv1, Tau::new(t));
        }
      }
    }
    (Ty::Fun(arg_ty1, res_ty1), Ty::Fun(arg_ty2, res_ty2)) => {
      unify(cx, entity, *arg_ty1, *arg_ty2);
      unify(cx, entity, *res_ty1, *res_ty2);
    }
    (ty1, ty2) => {
      cx.err(entity, EK::CannotUnify(ty1, ty2));
    }
  }
}

pub(crate) fn unify_fn(cx: &mut Cx, entity: E, ty: Rho) -> (Ty, Rho) {
  match ty.into_inner() {
    Ty::Fun(arg_ty, res_ty) => (*arg_ty, Rho::new(*res_ty)),
    ty => {
      let arg_ty = Ty::MetaTyVar(cx.new_meta_ty_var());
      let res_ty = Ty::MetaTyVar(cx.new_meta_ty_var());
      let fn_ty = Ty::fun(arg_ty.clone(), res_ty.clone());
      unify(cx, entity, fn_ty, ty);
      (arg_ty, Rho::new(res_ty))
    }
  }
}
