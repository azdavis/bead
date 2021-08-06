use identifier_case::snake_to_pascal;
use syntax_gen::{gen, TokenKind};

fn main() -> std::io::Result<()> {
  gen(
    "Bidir",
    &["Invalid", "Whitespace", "LineComment"],
    include_str!("syntax.ungram").parse().unwrap(),
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
          let mut s = String::new();
          for c in tok.chars() {
            s.push_str(char_name::get(c));
          }
          (TokenKind::Punctuation, s)
        }
      }
    },
  )
}
