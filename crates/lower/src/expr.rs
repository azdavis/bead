use crate::ty;
use crate::util::Lower as Cx;
use hir::Name;
use syntax::{ast::Expr, AstPtr};

pub(crate) fn get(cx: &mut Cx, expr: Expr) -> hir::ExprIdx {
  let ptr = AstPtr::new(&expr);
  let hir_expr = get_(cx, expr).unwrap_or(hir::Expr::None);
  let ret = cx.arenas.expr.alloc(hir_expr);
  cx.ptrs.expr_fwd.insert(ret, ptr);
  cx.ptrs.expr_back.insert(ptr, ret);
  ret
}

fn get_(cx: &mut Cx, expr: Expr) -> Option<hir::Expr> {
  let ret = match expr {
    Expr::ParenExpr(expr) => get_(cx, expr.expr()?)?,
    Expr::IntExpr(expr) => hir::Expr::Int(expr.int_lit()?.text().parse().ok()?),
    Expr::NameExpr(expr) => hir::Expr::Name(Name::new(expr.name()?.text())),
    Expr::LamExpr(expr) => {
      let var = Name::new(expr.name()?.text());
      let body = get(cx, expr.expr()?);
      hir::Expr::Lam(var, body)
    }
    Expr::ALamExpr(expr) => {
      let var = Name::new(expr.name()?.text());
      let t = ty::get(cx, expr.ty()?);
      let body = get(cx, expr.expr()?);
      hir::Expr::ALam(var, t, body)
    }
    Expr::AppExpr(expr) => {
      let lhs = get(cx, expr.lhs()?);
      let rhs = get(cx, expr.rhs()?);
      hir::Expr::App(lhs, rhs)
    }
    Expr::LetExpr(expr) => {
      let var = Name::new(expr.name()?.text());
      let var_def = get(cx, expr.var_def()?);
      let body = get(cx, expr.body()?);
      hir::Expr::Let(var, var_def, body)
    }
    Expr::AnnExpr(expr) => {
      let e = get(cx, expr.expr()?);
      let t = ty::get(cx, expr.ty()?);
      hir::Expr::Ann(e, t)
    }
  };
  Some(ret)
}
