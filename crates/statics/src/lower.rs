use crate::defs::{
  BoundTyVar, Cx, Entity as E, ErrorKind as EK, Rho, Ty, TyVar,
};
use hir::{Arenas, TyIdx};

pub(crate) fn ty(cx: &mut Cx, arenas: &Arenas, ty_idx: TyIdx) -> Ty {
  match arenas.ty[ty_idx] {
    hir::Ty::None => Ty::None,
    hir::Ty::ForAll(ref tvs, t) => {
      let tvs: Vec<_> = tvs
        .iter()
        .map(|name| BoundTyVar::new(name.clone()))
        .collect();
      let t = Rho::new_opt(ty(cx, arenas, t)).unwrap_or_else(|| {
        cx.err(E::Ty(t), EK::InvalidRhoTy);
        Rho::new(Ty::None)
      });
      Ty::for_all(tvs, t)
    }
    hir::Ty::Fun(arg, res) => {
      let arg = ty(cx, arenas, arg);
      let res = ty(cx, arenas, res);
      Ty::fun(arg, res)
    }
    hir::Ty::Int => Ty::Int,
    hir::Ty::Str => Ty::Str,
    hir::Ty::Name(ref name) => {
      Ty::TyVar(TyVar::Bound(BoundTyVar::new(name.clone())))
    }
  }
}
