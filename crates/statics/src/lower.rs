use crate::defs::{self, BoundTyVar, Cx, Rho, TyVar};
use hir::{Arenas, Name, Ty, TyIdx};

pub(crate) fn ty(cx: &mut Cx, arenas: &Arenas, ty_idx: TyIdx) -> defs::Ty {
  match arenas.ty[ty_idx] {
    Ty::None => defs::Ty::None,
    Ty::ForAll(ref tvs, t) => {
      let tvs: Vec<_> = tvs.iter().map(|name| bound_ty_var(cx, name)).collect();
      // TODO `Rho::new` could panic.
      let t = Rho::new(ty(cx, arenas, t));
      defs::Ty::for_all(tvs, t)
    }
    Ty::Fun(arg, res) => {
      let arg = ty(cx, arenas, arg);
      let res = ty(cx, arenas, res);
      defs::Ty::fun(arg, res)
    }
    Ty::Int => defs::Ty::Int,
    Ty::Name(ref name) => defs::Ty::TyVar(TyVar::Bound(bound_ty_var(cx, name))),
  }
}

fn bound_ty_var(cx: &mut Cx, name: &Name) -> BoundTyVar {
  BoundTyVar::new(cx.name(name))
}
