use crate::defs::{
  self, BoundTyVar, Cx, Entity as E, ErrorKind as EK, Rho, TyVar,
};
use hir::{Arenas, Ty, TyIdx};

pub(crate) fn ty(cx: &mut Cx, arenas: &Arenas, ty_idx: TyIdx) -> defs::Ty {
  match arenas.ty[ty_idx] {
    Ty::None => defs::Ty::None,
    Ty::ForAll(ref tvs, t) => {
      let tvs: Vec<_> = tvs
        .iter()
        .map(|name| BoundTyVar::new(name.clone()))
        .collect();
      let t = Rho::new_opt(ty(cx, arenas, t)).unwrap_or_else(|| {
        cx.err(E::Ty(t), EK::InvalidRhoTy);
        Rho::new(defs::Ty::None)
      });
      defs::Ty::for_all(tvs, t)
    }
    Ty::Fun(arg, res) => {
      let arg = ty(cx, arenas, arg);
      let res = ty(cx, arenas, res);
      defs::Ty::fun(arg, res)
    }
    Ty::Int => defs::Ty::Int,
    Ty::Name(ref name) => {
      defs::Ty::TyVar(TyVar::Bound(BoundTyVar::new(name.clone())))
    }
  }
}
