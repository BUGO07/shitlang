use crate::token::{Literal, Operator, Token};

pub enum Stmt {
    Expr(Expr),
    Declaration { name: String, value: Expr },
    Scope { statements: Vec<Stmt> },
    While { condition: Expr, body: Vec<Stmt> },
    Return { value: Option<Expr> },
    Break,
    Continue,
}

pub enum Expr {
    Literal(Literal),
    Variable(String),
    Binary {
        left: Box<Expr>,
        operator: Operator,
        right: Box<Expr>,
    },
    Unary {
        operator: Operator,
        operand: Box<Expr>,
    },
    FunctionCall {
        name: String,
        arguments: Vec<Expr>,
    },
    Assignment {
        target: Box<Expr>,
        value: Box<Expr>,
    },
    If {
        condition: Box<Expr>,
        then_branch: Vec<Stmt>,
        else_branch: Option<Vec<Stmt>>,
    },
}

pub struct AbstractSyntaxTree {
    pub tokens: Vec<Token>,
    pub current_index: usize,
    pub global_scope: Vec<Stmt>,
}

impl AbstractSyntaxTree {
    pub fn new(tokens: Vec<Token>) -> Self {
        AbstractSyntaxTree {
            tokens,
            current_index: 0,
            global_scope: Vec::new(),
        }
    }

    fn peek(&self) -> &Token {
        self.tokens.get(self.current_index).unwrap()
    }

    fn advance(&mut self) -> &Token {
        self.current_index += 1;
        self.tokens.get(self.current_index - 1).unwrap()
    }

    pub fn build(self) -> anyhow::Result<Self> {
        Ok(self)
    }
}
