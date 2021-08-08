pub(crate) fn check(s: &str) {
  let lexed = lex::get(s);
  assert!(lexed.errors.is_empty());
  let parsed = parse::get(&lexed.tokens);
  assert!(parsed.errors.is_empty());
  let lowered = lower_neue::get(parsed.root);
  assert_eq!(lowered.top.len(), 1);
  let expr = *lowered.top.first().unwrap();
  statics::get(&lowered.arenas, expr);
}
