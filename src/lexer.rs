use crate::token::{
    Delimiter, Keyword, Literal, Location, NumericType, Operator, Token, TokenType,
};

pub struct Lexer {
    source: String,
    current_loc: Location,
    tokens: Vec<Token>,
}

impl Lexer {
    pub fn new(source: String) -> Self {
        Lexer {
            source,
            current_loc: Location {
                line: 1,
                column: 0,
                index: 0,
            },
            tokens: Vec::new(),
        }
    }

    fn advance(&mut self, n: usize) {
        self.current_loc.index += n;
        self.current_loc.column += n;
    }

    pub fn tokens(&self) -> &Vec<Token> {
        &self.tokens
    }

    pub fn location(&self) -> &Location {
        &self.current_loc
    }

    fn numeric_literal(&mut self, ch: char, chars: &[char]) -> anyhow::Result<()> {
        let start_loc = self.current_loc;
        let mut literal = String::new();
        literal.push(ch);

        let mut has_dot = false;

        while let Some(&c) = chars.get(self.current_loc.index) {
            match c {
                '0'..='9' => {
                    self.advance(1);
                    literal.push(c);
                }
                '_' => {
                    self.advance(1);
                }
                '.' => {
                    anyhow::ensure!(!has_dot, "Invalid numeric literal: multiple decimal points");
                    has_dot = true;
                    self.advance(1);
                    literal.push('.');
                }
                _ => break,
            }
        }

        let mut suffix = String::new();

        while let Some(&c) = chars.get(self.current_loc.index) {
            match c {
                'i' | 'u' | 'f' | 's' => {
                    self.advance(1);
                    suffix.push(c);
                }
                '0'..='9' => {
                    if suffix.is_empty() {
                        break;
                    }
                    self.advance(1);
                    suffix.push(c);
                }
                _ => break,
            }
        }

        let numeric_type = if suffix.is_empty() {
            if has_dot {
                NumericType::F64
            } else {
                NumericType::I32
            }
        } else {
            match suffix.as_str() {
                "i8" => NumericType::I8,
                "i16" => NumericType::I16,
                "i32" => NumericType::I32,
                "i64" => NumericType::I64,
                "is" => NumericType::ISize,
                "u8" => NumericType::U8,
                "u16" => NumericType::U16,
                "u32" => NumericType::U32,
                "u64" => NumericType::U64,
                "us" => NumericType::USize,
                "f32" => NumericType::F32,
                "f64" => NumericType::F64,
                _ => anyhow::bail!("Unknown numeric type suffix '{}'", suffix),
            }
        };

        anyhow::ensure!(
            !has_dot || matches!(numeric_type, NumericType::F32 | NumericType::F64),
            "Invalid numeric literal: integer literal cannot contain '.'",
        );

        self.tokens.push(Token::new(
            TokenType::Literal(Literal::Numeric(literal + "_" + &suffix)),
            start_loc,
        ));
        Ok(())
    }

    pub fn tokenize(&mut self) -> anyhow::Result<()> {
        let chars = self.source.chars().collect::<Vec<_>>();

        while let Some(&ch) = chars.get(self.current_loc.index) {
            self.advance(1);

            match ch {
                ' ' | '\r' => {}
                '\t' => {
                    self.current_loc.column += 3;
                }
                '\n' => {
                    self.current_loc.line += 1;
                    self.current_loc.column = 0;
                }

                '{' | '}' | '(' | ')' | '[' | ']' | ';' | ':' | ',' | '.' | '=' => {
                    if ch == '=' {
                        if let Some(&next) = chars.get(self.current_loc.index) {
                            match next {
                                '=' => {
                                    self.advance(1);
                                    self.tokens.push(Token::new(
                                        TokenType::Operator(Operator::Equals),
                                        self.current_loc,
                                    ));
                                    continue;
                                }
                                '>' => {
                                    self.advance(1);
                                    self.tokens.push(Token::new(
                                        TokenType::Delimiter(Delimiter::Arrow),
                                        self.current_loc,
                                    ));
                                    continue;
                                }
                                _ => {
                                    self.tokens.push(Token::new(
                                        TokenType::Operator(Operator::Assign),
                                        self.current_loc,
                                    ));
                                    continue;
                                }
                            }
                        } else {
                            anyhow::bail!("Unexpected '=' character");
                        }
                    } else if ch == '.'
                        && let Some(&next) = chars.get(self.current_loc.index)
                        && next == '.'
                    {
                        self.advance(1);
                        self.tokens.push(Token::new(
                            TokenType::Delimiter(Delimiter::Variadic),
                            self.current_loc,
                        ));
                        continue;
                    }

                    let delimiter = match ch {
                        '{' => Delimiter::LBrace,
                        '}' => Delimiter::RBrace,
                        '(' => Delimiter::LParen,
                        ')' => Delimiter::RParen,
                        '[' => Delimiter::LBracket,
                        ']' => Delimiter::RBracket,
                        ';' => Delimiter::Semicolon,
                        ':' => Delimiter::Colon,
                        ',' => Delimiter::Comma,
                        '.' => Delimiter::Dot,
                        _ => unreachable!(),
                    };

                    self.tokens.push(Token::new(
                        TokenType::Delimiter(delimiter),
                        self.current_loc,
                    ));
                }

                '+' | '-' | '*' | '/' | '%' | '!' | '>' | '<' | '&' | '|' => {
                    if let Some(&next) = chars.get(self.current_loc.index) {
                        let operator = match next {
                            '=' => {
                                self.advance(1);
                                match ch {
                                    '+' => Operator::AddAssign,
                                    '-' => Operator::SubAssign,
                                    '*' => Operator::MulAssign,
                                    '/' => Operator::DivAssign,
                                    '%' => Operator::ModAssign,
                                    '!' => Operator::NotEquals,
                                    '>' => Operator::GreaterEquals,
                                    '<' => Operator::LessEquals,
                                    '&' => Operator::BitAndAssign,
                                    '|' => Operator::BitOrAssign,
                                    _ => unreachable!(),
                                }
                            }
                            '/' => match ch {
                                '/' => {
                                    while let Some(&c) = chars.get(self.current_loc.index) {
                                        self.advance(1);
                                        if c == '\n' {
                                            self.current_loc.line += 1;
                                            self.current_loc.column = 0;
                                            break;
                                        }
                                    }
                                    continue;
                                }
                                _ => anyhow::bail!("Unexpected '/' character"),
                            },
                            '*' => match ch {
                                '/' => {
                                    while let Some(&c) = chars.get(self.current_loc.index) {
                                        self.advance(1);
                                        match c {
                                            '\n' => {
                                                self.current_loc.line += 1;
                                                self.current_loc.column = 0;
                                            }
                                            '*' => {
                                                if let Some('/') = chars.get(self.current_loc.index)
                                                {
                                                    self.advance(1);
                                                    break;
                                                }
                                            }
                                            _ => {}
                                        }
                                    }
                                    continue;
                                }
                                _ => Operator::Asterisk,
                            },
                            '&' | '|' => {
                                if ch == next {
                                    self.advance(1);
                                    match ch {
                                        '&' => Operator::LogicalAnd,
                                        '|' => Operator::LogicalOr,
                                        x => anyhow::bail!(
                                            "Unexpected '{next}' character after '{x}'"
                                        ),
                                    }
                                } else {
                                    match ch {
                                        '&' => Operator::Ampersand,
                                        '|' => Operator::Pipe,
                                        x => anyhow::bail!(
                                            "Unexpected '{next}' character after '{x}'"
                                        ),
                                    }
                                }
                            }
                            x => match ch {
                                '+' => Operator::Plus,
                                '-' => {
                                    if x.is_ascii_digit() {
                                        self.numeric_literal(ch, &chars)?;
                                        continue;
                                    } else {
                                        Operator::Minus
                                    }
                                }
                                '*' => Operator::Asterisk,
                                '/' => Operator::Slash,
                                '%' => Operator::Percent,
                                '!' => Operator::Exclem,
                                '>' => Operator::Greater,
                                '<' => Operator::Less,
                                '&' => Operator::Ampersand,
                                '|' => Operator::Pipe,
                                _ => unreachable!(),
                            },
                        };

                        self.tokens
                            .push(Token::new(TokenType::Operator(operator), self.current_loc));
                    } else {
                        anyhow::bail!("Unexpected operator character");
                    }
                }

                '0'..='9' => {
                    self.numeric_literal(ch, &chars)?;
                }

                '"' => {
                    let mut s = String::new();

                    while let Some(&c) = chars.get(self.current_loc.index) {
                        self.advance(1);
                        if c == '"' {
                            break;
                        }
                        s.push(c);
                    }

                    self.tokens.push(Token::new(
                        TokenType::Literal(Literal::String(s)),
                        self.current_loc,
                    ));
                }

                'a'..='z' | 'A'..='Z' | '_' => {
                    let mut ident = String::new();
                    ident.push(ch);

                    while let Some(&c) = chars.get(self.current_loc.index) {
                        match c {
                            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                                self.advance(1);
                                ident.push(c);
                            }
                            _ => break,
                        }
                    }

                    let token = match ident.as_str() {
                        "let" => TokenType::Keyword(Keyword::Let),
                        "func" => TokenType::Keyword(Keyword::Func),
                        "return" => TokenType::Keyword(Keyword::Return),
                        "if" => TokenType::Keyword(Keyword::If),
                        "else" => TokenType::Keyword(Keyword::Else),
                        "while" => TokenType::Keyword(Keyword::While),
                        "for" => TokenType::Keyword(Keyword::For),
                        "break" => TokenType::Keyword(Keyword::Break),
                        "continue" => TokenType::Keyword(Keyword::Continue),
                        "extern" => TokenType::Keyword(Keyword::Extern),
                        "import" => TokenType::Keyword(Keyword::Import),
                        "as" => TokenType::Keyword(Keyword::As),
                        "true" | "false" => TokenType::Literal(Literal::Boolean(ident)),
                        _ => TokenType::Identifier(ident),
                    };

                    self.tokens.push(Token::new(token, self.current_loc));
                }

                _ => {
                    anyhow::bail!("Unexpected character '{}'", ch);
                }
            }
        }

        Ok(())
    }
}
