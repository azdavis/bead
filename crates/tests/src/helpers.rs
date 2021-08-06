use defs::{Cx, Env};

pub(crate) fn check(s: &str) {
  let lexed = lex::get(s);
  assert!(lexed.errors.is_empty());
  let parsed = parse::get(&lexed.tokens);
  assert!(parsed.errors.is_empty());
  let mut lowered = lower::get(parsed.root);
  let expr = lowered.exprs.pop().unwrap();
  assert!(lowered.exprs.is_empty());
  let mut cx = Cx::default();
  let env = Env::default();
  statics::infer_ty(&mut cx, &env, &expr);
}
