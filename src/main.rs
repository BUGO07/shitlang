use std::time::Instant;

use crate::lexer::Lexer;

mod lexer;
mod token;

const CODE: &str = r#"
{"test string"}() [ 4.31 ]6. 5 == = => < <= >= != ~= "5asd" 5 && & true false if let func return asdhjzxckh else
"#;

fn main() {
    let mut lexer = Lexer::new(CODE.to_string());

    let time = Instant::now();
    lexer.tokenize().unwrap();

    println!(
        "{:?}, time it took: {:?}",
        lexer
            .tokens()
            .iter()
            .map(|x| &x.token_type)
            .collect::<Vec<_>>(),
        time.elapsed()
    );
}
