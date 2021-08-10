pub(crate) fn check(s: &str) {
  let lex_res = lex::get(s);
  assert!(lex_res.errors.is_empty(), "lex err");
  let pars_res = parse::get(&lex_res.tokens);
  assert!(pars_res.errors.is_empty(), "parse err");
  let lower_res = lower::get(pars_res.root);
  assert_eq!(lower_res.top.len(), 1, "bad expr count");
  let expr = *lower_res.top.first().expect("just checked len() == 1");
  let statics_res = statics::get(&lower_res.arenas, expr);
  assert!(statics_res.errors.is_empty(), "statics err");
}
