//! Tests.

#![cfg(test)]

mod defs;

use defs::check;

#[test]
fn int_lit() {
  check("3 : Int");
}

#[test]
fn str_lit() {
  check(
    r#"
"foo bar" : Str
  "#,
  );
}

#[test]
fn id_regular() {
  check("(fn x. x) : forall a. a -> a");
}

#[test]
fn id_int_annot() {
  check("(fn x: Int. x) : Int -> Int");
}

#[test]
fn let_1() {
  check("(let x = 3 in x) : Int");
}

#[test]
fn let_2() {
  check("(let x = 3 in let id = fn x. x in id id 3) : Int");
}

#[test]
fn id_id() {
  check("let id = fn x. x in id id : forall a. a -> a");
}

#[test]
fn app_int() {
  check("(fn f. f 3) : forall a. (Int -> a) -> a");
}

#[test]
fn many_id() {
  check(
    r#"
(let id = fn x. x in
let int_id = fn x: Int. x in
fn f. f id (f int_id 3)) :
((Int -> Int) -> Int -> Int) -> Int
"#,
  )
}

#[test]
#[should_panic = "statics err"]
fn not_in_scope() {
  check("x");
}

#[test]
#[should_panic = "statics err"]
fn bad_app() {
  check("3 3");
}

#[test]
#[should_panic = "statics err"]
fn bad_rho_ty() {
  check("fn x: (forall a. forall b. a). x");
}

#[test]
#[should_panic = "lex err"]
fn smoke_lex() {
  check("あ");
}

#[test]
#[should_panic = "parse err"]
fn smoke_parse() {
  check("(");
}

#[test]
#[should_panic = "statics err"]
fn occurs_check() {
  check("fn f. f f");
}
