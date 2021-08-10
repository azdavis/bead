pub(crate) fn check(s: &str) {
  let lex_res = lex::get(s);
  assert!(lex_res.errors.is_empty());
  let pars_res = parse::get(&lex_res.tokens);
  assert!(pars_res.errors.is_empty());
  let lower_res = lower::get(pars_res.root);
  assert_eq!(lower_res.top.len(), 1);
  let expr = *lower_res.top.first().unwrap();
  statics::get(&lower_res.arenas, expr);
}
