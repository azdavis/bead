//! Parse expressions.

use crate::ty::ty;
use event_parse::{Exited, Parser};
use syntax::SyntaxKind as SK;

pub(crate) fn expr(p: &mut Parser<'_, SK>) {
  let mut ex = match expr_hd_opt(p) {
    Some(x) => x,
    None => {
      p.error("expected an expression");
      return;
    }
  };
  loop {
    let en = p.precede(ex);
    let kind = if p.at(SK::Colon) {
      p.bump();
      ty(p);
      SK::AnnExpr
    } else if expr_hd_opt(p).is_some() {
      SK::AppExpr
    } else {
      p.abandon(en);
      break;
    };
    ex = p.exit(en, kind);
  }
}

fn expr_hd_opt(p: &mut Parser<'_, SK>) -> Option<Exited> {
  let en = p.enter();
  let kind = if p.at(SK::LRound) {
    p.bump();
    expr(p);
    p.eat(SK::RRound);
    SK::ParenExpr
  } else if p.at(SK::IntLit) {
    p.bump();
    SK::IntExpr
  } else if p.at(SK::StrLit) {
    p.bump();
    SK::StrExpr
  } else if p.at(SK::FnKw) {
    p.bump();
    p.eat(SK::Name);
    let is_ann = p.at(SK::Colon);
    if is_ann {
      p.bump();
      ty(p);
    }
    p.eat(SK::Dot);
    expr(p);
    if is_ann {
      SK::ALamExpr
    } else {
      SK::LamExpr
    }
  } else if p.at(SK::LetKw) {
    p.bump();
    p.eat(SK::Name);
    p.eat(SK::Eq);
    expr(p);
    p.eat(SK::InKw);
    expr(p);
    SK::LetExpr
  } else if p.at(SK::Name) {
    p.bump();
    SK::NameExpr
  } else {
    p.abandon(en);
    return None;
  };
  Some(p.exit(en, kind))
}
