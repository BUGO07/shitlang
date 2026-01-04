#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub ty: TokenType,
    pub location: Location,
}

impl Token {
    pub fn new(ty: TokenType, location: Location) -> Self {
        Token { ty, location }
    }
}
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Location {
    pub line: usize,
    pub column: usize,
    pub index: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    Identifier(String),
    Keyword(Keyword),
    Literal(Literal),
    Operator(Operator),
    Delimiter(Delimiter),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Numeric(String),
    Char(char),
    String(String),
    Boolean(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumericType {
    I8,
    I16,
    I32,
    I64,
    ISize,
    U8,
    U16,
    U32,
    U64,
    USize,
    F32,
    F64,
}

impl NumericType {
    pub fn from_literal(literal: &str) -> anyhow::Result<Option<Self>> {
        // Find the suffix after the underscore using rfind to avoid allocating a Vec
        let suffix = if let Some(pos) = literal.rfind('_') {
            &literal[pos + 1..]
        } else {
            ""
        };
        
        if suffix.is_empty() {
            Ok(None)
        } else {
            Ok(Some(match suffix {
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
            }))
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Let,
    Func,
    Return,
    If,
    Else,
    While,
    For,
    Break,
    Continue,
    Extern,
    // True,
    // False,
    Import,
    As,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    Plus,          // +
    Minus,         // -
    Asterisk,      // *
    Slash,         // /
    Percent,       // %
    Equals,        // ==
    NotEquals,     // !=
    Greater,       // >
    Less,          // <
    GreaterEquals, // >=
    LessEquals,    // <=
    LogicalAnd,    // &&
    LogicalOr,     // ||
    Exclem,        // !
    Ampersand,     // &
    Pipe,          // |
    Assign,        // =
    AddAssign,     // +=
    SubAssign,     // -=
    MulAssign,     // *=
    DivAssign,     // /=
    ModAssign,     // %=
    BitAndAssign,  // &=
    BitOrAssign,   // |=
}

impl Operator {
    pub fn rank(&self) -> u8 {
        match self {
            Operator::Exclem => 9,
            Operator::Asterisk | Operator::Slash | Operator::Percent => 8,
            Operator::Plus | Operator::Minus => 7,
            Operator::Greater | Operator::Less | Operator::GreaterEquals | Operator::LessEquals => {
                6
            }
            Operator::Equals | Operator::NotEquals => 5,
            Operator::Ampersand => 4,
            Operator::Pipe => 3,
            Operator::LogicalAnd => 2,
            Operator::LogicalOr => 1,
            _ => 0,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Delimiter {
    LBrace,    // {
    RBrace,    // }
    LParen,    // (
    RParen,    // )
    LBracket,  // [
    RBracket,  // ]
    Semicolon, // ;
    Colon,     // :
    Comma,     // ,
    Dot,       // .
    Arrow,     // =>
    Variadic,  // ..
}
