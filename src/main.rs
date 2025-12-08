use crate::lexer::Lexer;

mod lexer;
mod token;

const CODE: &str = r#"
{"test string"}() 4.31 5 == = => < <= >= != ~=

"#;

fn main() {
    let mut lexer = Lexer::new(CODE.to_string());
    lexer.tokenize().unwrap();

    println!(
        "{:?}",
        lexer
            .tokens()
            .iter()
            .map(|x| &x.token_type)
            .collect::<Vec<_>>()
    );
}
