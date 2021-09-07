use hir::{Arenas, ExprIdx};

fn lower_one(s: &str) -> (ExprIdx, Arenas) {
  let lex_res = lex::get(s);
  assert!(lex_res.errors.is_empty(), "lex err");
  let parse_res = parse::get(&lex_res.tokens);
  assert!(parse_res.errors.is_empty(), "parse err");
  let lower_res = lower::get(parse_res.root);
  assert_eq!(lower_res.top.len(), 1, "bad expr count");
  let expr = *lower_res.top.first().expect("just checked len() == 1");
  (expr, lower_res.arenas)
}

pub(crate) fn check(s: &str) {
  let (expr, arenas) = lower_one(s);
  let statics_res = statics::get(&arenas, expr);
  assert!(statics_res.errors.is_empty(), "statics err");
}
