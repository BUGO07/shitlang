#[derive(Debug)]
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

#[derive(Clone, Copy, Debug)]
pub struct Location {
    pub line: usize,
    pub column: usize,
    pub index: usize,
}

#[derive(Debug)]
pub enum TokenType {
    Identifier(String),
    Keyword(Keyword),
    Literal(Literal),
    Operator(Operator),
    Delimiter(Delimiter),
}

#[derive(Debug)]
pub enum Literal {
    Numeric(String, NumericType),
    String(String),
    Boolean(String),
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Keyword {
    Let,
    Func,
    Return,
    If,
    Else,
    ElseIf,
    While,
    For,
    Break,
    Continue,
    True,
    False,
    Import,
    As,
}

#[derive(Debug)]
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

#[derive(Debug)]
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
