//! Parse types.

use event_parse::Parser;
use syntax::SyntaxKind as SK;

pub(crate) fn ty(p: &mut Parser<'_, SK>) {
  let en = p.enter();
  let kind = if p.at(SK::LRound) {
    p.bump();
    ty(p);
    p.eat(SK::RRound);
    SK::ParenTy
  } else if p.at(SK::ForallKw) {
    p.bump();
    while p.at(SK::Name) {
      p.bump();
    }
    p.eat(SK::Dot);
    ty(p);
    SK::ForAllTy
  } else if p.at(SK::IntKw) {
    p.bump();
    SK::IntTy
  } else if p.at(SK::Name) {
    p.bump();
    SK::NameTy
  } else {
    p.abandon(en);
    p.error();
    return;
  };
  let ex = p.exit(en, kind);
  if p.at(SK::Arrow) {
    let en = p.precede(ex);
    p.bump();
    ty(p);
    p.exit(en, SK::FnTy);
  }
}
