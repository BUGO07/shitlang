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

    pub fn tokenize(&mut self) -> Result<(), String> {
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
                            return Err("Unexpected '=' character".into());
                        }
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

                '+' | '-' | '*' | '/' | '%' | '!' | '>' | '<' | '&' | '|' | '~' => {
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
                                    '~' => Operator::BitNotAssign,
                                    _ => unreachable!(),
                                }
                            }
                            '/' => {
                                if ch == '/' {
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
                                return Err("Unexpected '/' character".into());
                            }
                            '&' | '|' => {
                                if ch == next {
                                    self.advance(1);
                                    match ch {
                                        '&' => Operator::LogicalAnd,
                                        '|' => Operator::LogicalOr,
                                        _ => unreachable!(),
                                    }
                                } else {
                                    match ch {
                                        '&' => Operator::BitAnd,
                                        '|' => Operator::BitOr,
                                        _ => unreachable!(),
                                    }
                                }
                            }
                            _ => match ch {
                                '+' => Operator::Plus,
                                '-' => Operator::Minus,
                                '*' => Operator::Multiply,
                                '/' => Operator::Divide,
                                '%' => Operator::Modulus,
                                '!' => Operator::LogicalNot,
                                '>' => Operator::Greater,
                                '<' => Operator::Less,
                                '&' => Operator::BitAnd,
                                '|' => Operator::BitOr,
                                '~' => Operator::BitNot,
                                _ => unreachable!(),
                            },
                        };

                        self.tokens
                            .push(Token::new(TokenType::Operator(operator), self.current_loc));
                    } else {
                        return Err("Unexpected operator character".into());
                    }
                }

                '0'..='9' => {
                    let mut num = String::new();
                    num.push(ch);

                    while let Some(&c) = chars.get(self.current_loc.index) {
                        match c {
                            '0'..='9' | '.' => {
                                self.advance(1);
                                num.push(c);
                            }
                            ' ' | '\n' | '\r' | '\t' => break,
                            _ => return Err("Unexpected character after numeral".into()),
                        }
                    }

                    let float = num.contains('.');
                    self.tokens.push(Token::new(
                        TokenType::Literal(Literal::Numeric(
                            num,
                            if float {
                                NumericType::F64
                            } else {
                                NumericType::I32
                            },
                        )),
                        self.current_loc,
                    ));
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
                        "import" => TokenType::Keyword(Keyword::Import),
                        "as" => TokenType::Keyword(Keyword::As),
                        "true" | "false" => TokenType::Literal(Literal::Boolean(ident)),
                        _ => TokenType::Identifier(ident),
                    };

                    self.tokens.push(Token::new(token, self.current_loc));
                }

                _ => {}
            }
        }

        Ok(())
    }
}
