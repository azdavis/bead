use crate::defs::Lower as Cx;
use hir::Name;
use syntax::{ast::Ty, AstPtr};

pub(crate) fn get(cx: &mut Cx, ty: Ty) -> hir::TyIdx {
  let ptr = AstPtr::new(&ty);
  let hir_ty = get_(cx, Some(ty)).unwrap_or(hir::Ty::None);
  let ret = cx.arenas.ty.alloc(hir_ty);
  cx.ptrs.ty_fwd.insert(ret, ptr);
  cx.ptrs.ty_back.insert(ptr, ret);
  ret
}

fn get_(cx: &mut Cx, ty: Option<Ty>) -> Option<hir::Ty> {
  let ret = match ty? {
    Ty::ParenTy(ty) => get_(cx, ty.ty())?,
    Ty::IntTy(_) => hir::Ty::Int,
    Ty::StrTy(_) => hir::Ty::Str,
    Ty::NameTy(ty) => hir::Ty::Name(Name::new(ty.name()?.text())),
    Ty::ForAllTy(ty) => {
      let tvs: Vec<_> = ty.names().map(|tok| Name::new(tok.text())).collect();
      let t = get(cx, ty.ty()?);
      hir::Ty::ForAll(tvs, t)
    }
    Ty::FnTy(ty) => {
      let lhs = get(cx, ty.lhs()?);
      let rhs = get(cx, ty.rhs()?);
      hir::Ty::Fun(lhs, rhs)
    }
  };
  Some(ret)
}
