#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub token_type: TokenType,
    pub location: Location,
}

impl Token {
    pub fn new(token_type: TokenType, location: Location) -> Self {
        Token {
            token_type,
            location,
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    Numeric(String, NumericType),
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
    True,
    False,
    Import,
    As,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    Plus,          // +
    Minus,         // -
    Multiply,      // *
    Divide,        // /
    Modulus,       // %
    Equals,        // =
    NotEquals,     // !=
    Greater,       // >
    Less,          // <
    GreaterEquals, // >=
    LessEquals,    // <=
    LogicalAnd,    // &&
    LogicalOr,     // ||
    LogicalNot,    // !
    BitAnd,        // &
    BitOr,         // |
    BitNot,        // ~
    Assign,        // =
    AddAssign,     // +=
    SubAssign,     // -=
    MulAssign,     // *=
    DivAssign,     // /=
    ModAssign,     // %=
    BitAndAssign,  // &=
    BitOrAssign,   // |=
    BitNotAssign,  // ~=
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
}
