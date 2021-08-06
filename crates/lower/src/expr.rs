use crate::ty;
use crate::util::Cx;
use syntax::ast::Expr;

pub(crate) fn get(cx: &mut Cx, expr: Expr) -> Option<defs::Expr> {
  let ret = match expr {
    Expr::ParenExpr(expr) => get(cx, expr.expr()?)?,
    Expr::IntExpr(expr) => {
      defs::Expr::Int(expr.int_lit()?.text().parse().ok()?)
    }
    Expr::NameExpr(expr) => defs::Expr::Var(cx.name(expr.name()?.text())),
    Expr::LamExpr(expr) => {
      let var = cx.name(expr.name()?.text());
      let body = Box::new(get(cx, expr.expr()?)?);
      defs::Expr::Lam(var, body)
    }
    Expr::ALamExpr(expr) => {
      let var = cx.name(expr.name()?.text());
      let t = ty::get(cx, expr.ty()?)?;
      let body = Box::new(get(cx, expr.expr()?)?);
      defs::Expr::ALam(var, t, body)
    }
    Expr::AppExpr(expr) => {
      let lhs = Box::new(get(cx, expr.lhs()?)?);
      let rhs = Box::new(get(cx, expr.rhs()?)?);
      defs::Expr::App(lhs, rhs)
    }
    Expr::LetExpr(expr) => {
      let var = cx.name(expr.name()?.text());
      let var_def = Box::new(get(cx, expr.var_def()?)?);
      let body = Box::new(get(cx, expr.body()?)?);
      defs::Expr::Let(var, var_def, body)
    }
    Expr::AnnExpr(expr) => {
      let e = Box::new(get(cx, expr.expr()?)?);
      let t = ty::get(cx, expr.ty()?)?;
      defs::Expr::Ann(e, t)
    }
  };
  Some(ret)
}
