use syntax_gen::{
  char_name::get, gen, identifier_case::snake_to_pascal, TokenKind,
};

fn main() -> std::io::Result<()> {
  gen(
    "Bead",
    &["Invalid", "Whitespace", "LineComment"],
    include_str!("syntax.ungram")
      .parse()
      .expect("ungram parse error"),
    |tok| match tok {
      "IntLit" => (TokenKind::Special("an integer literal"), tok.to_owned()),
      "Name" => (TokenKind::Special("a name"), tok.to_owned()),
      "->" => (TokenKind::Punctuation, "Arrow".to_owned()),
      _ => {
        if tok.chars().all(|c| c.is_ascii_alphabetic()) {
          let mut s = snake_to_pascal(tok);
          s.push_str("Kw");
          (TokenKind::Keyword, s)
        } else {
          (TokenKind::Punctuation, tok.chars().map(get).collect())
        }
      }
    },
  )
}
