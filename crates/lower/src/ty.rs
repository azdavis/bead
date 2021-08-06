use crate::util::Cx;
use defs::{BoundTyVar, Rho, TyVar};
use syntax::ast::Ty;

pub(crate) fn get(cx: &mut Cx, ty: Ty) -> Option<defs::Ty> {
  let ret = match ty {
    Ty::ParenTy(ty) => get(cx, ty.ty()?)?,
    Ty::IntTy(_) => defs::Ty::Int,
    Ty::NameTy(ty) => {
      defs::Ty::TyVar(TyVar::Bound(BoundTyVar::new(cx.name(ty.name()?.text()))))
    }
    Ty::ForAllTy(ty) => {
      let tvs: Vec<_> = ty
        .names()
        .map(|tok| BoundTyVar::new(cx.name(tok.text())))
        .collect();
      // TODO `Rho::new` could panic.
      let t = Rho::new(get(cx, ty.ty()?)?);
      defs::Ty::for_all(tvs, t)
    }
    Ty::FnTy(ty) => {
      let lhs = get(cx, ty.lhs()?)?;
      let rhs = get(cx, ty.rhs()?)?;
      defs::Ty::fun(lhs, rhs)
    }
  };
  Some(ret)
}
