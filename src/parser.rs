use crate::token::{
    Delimiter, Keyword, Literal, Location, NumericType, Operator, Token, TokenType,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Expr(Expr),
    Let {
        name: String,
        ty: Option<Type>,
        value: Expr,
    },
    Func {
        name: String,
        params: Vec<Param>,
        ty: Type,
        body: Box<Statement>,
    },
    Scope {
        statements: Vec<Statement>,
    },
    While {
        condition: Expr,
        body: Box<Statement>,
    },
    Return {
        value: Option<Expr>,
    },
    Extern {
        name: String,
        params: Vec<Param>,
        ty: Type,
    },
    Break,
    Continue,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Statement {
    pub stmt: Stmt,
    pub location: Location,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
        then_branch: Vec<Statement>,
        else_branch: Option<Vec<Statement>>,
    },
}

#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expression {
    pub expr: Expr,
    pub location: Location,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Named(String),
    Numeric(NumericType),
    Pointer(Box<Type>),
    Boolean,
    Variadic,
    Void,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: String,
    pub ty: Type,
}

pub struct Parser {
    pub tokens: Vec<Token>,
    pub current_index: usize,
    pub global_scope: Vec<Statement>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            current_index: 0,
            global_scope: Vec::new(),
        }
    }

    fn peek(&self) -> anyhow::Result<&Token> {
        if self.is_at_end() {
            anyhow::bail!(
                "Unexpected end of input at {:?}",
                self.tokens.last().unwrap().location
            );
        }
        Ok(self.tokens.get(self.current_index).unwrap())
    }

    fn advance(&mut self) -> anyhow::Result<&Token> {
        if self.is_at_end() {
            anyhow::bail!(
                "Unexpected end of input at {:?}",
                self.tokens.last().unwrap().location
            );
        }
        self.current_index += 1;
        Ok(self.tokens.get(self.current_index - 1).unwrap())
    }

    fn is_at_end(&self) -> bool {
        self.current_index >= self.tokens.len()
    }

    fn expect_delim(&mut self, expected: Delimiter) -> anyhow::Result<&Token> {
        let current = self.peek()?;
        anyhow::ensure!(
            matches!(current.ty, TokenType::Delimiter(d) if d == expected),
            "Expected delimiter {:?} at {:?}, found {:?}",
            expected,
            current.location,
            current.ty
        );

        self.advance()
    }

    fn parse_expr(&mut self) -> anyhow::Result<Expr> {
        self.parse_assignment()
    }

    fn parse_expr_stmt(&mut self) -> anyhow::Result<Stmt> {
        let expr = self.parse_expr()?;
        self.expect_delim(Delimiter::Semicolon)?;
        Ok(Stmt::Expr(expr))
    }

    fn parse_assignment(&mut self) -> anyhow::Result<Expr> {
        let expr = self.parse_binary(1)?;

        match self.peek()?.ty {
            TokenType::Operator(Operator::Assign) => {
                self.advance()?;
                let value = self.parse_assignment()?;
                return Ok(Expr::Assignment {
                    target: Box::new(expr),
                    value: Box::new(value),
                });
            }
            TokenType::Operator(
                op @ (Operator::AddAssign
                | Operator::SubAssign
                | Operator::MulAssign
                | Operator::DivAssign
                | Operator::ModAssign
                | Operator::BitAndAssign
                | Operator::BitOrAssign),
            ) => {
                self.advance()?;
                let right = self.parse_assignment()?;

                let binary_op = match op {
                    Operator::AddAssign => Operator::Plus,
                    Operator::SubAssign => Operator::Minus,
                    Operator::MulAssign => Operator::Asterisk,
                    Operator::DivAssign => Operator::Slash,
                    Operator::ModAssign => Operator::Percent,
                    Operator::BitAndAssign => Operator::Ampersand,
                    Operator::BitOrAssign => Operator::Pipe,
                    _ => unreachable!(),
                };

                return Ok(Expr::Assignment {
                    target: Box::new(expr.clone()),
                    value: Box::new(Expr::Binary {
                        left: Box::new(expr),
                        operator: binary_op,
                        right: Box::new(right),
                    }),
                });
            }
            _ => {}
        }

        Ok(expr)
    }

    fn parse_binary(&mut self, min_rank: u8) -> anyhow::Result<Expr> {
        let mut left = self.parse_unary()?;

        while let TokenType::Operator(op) = self.peek()?.ty {
            let rank = op.rank();
            if rank < min_rank {
                break;
            }

            let operator = if let TokenType::Operator(op) = self.advance()?.ty {
                op
            } else {
                unreachable!()
            };

            let right = self.parse_binary(rank + 1)?;
            left = Expr::Binary {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_unary(&mut self) -> anyhow::Result<Expr> {
        if let TokenType::Operator(
            operator @ (Operator::Minus
            | Operator::Exclem
            | Operator::Asterisk
            | Operator::Ampersand),
        ) = self.peek()?.ty
        {
            self.advance()?;

            let operand = self.parse_unary()?;
            return Ok(Expr::Unary {
                operator,
                operand: Box::new(operand),
            });
        }

        self.parse_primary()
    }

    fn parse_primary(&mut self) -> anyhow::Result<Expr> {
        match self.advance()?.ty.clone() {
            TokenType::Literal(lit) => Ok(Expr::Literal(lit)),

            TokenType::Identifier(name) => {
                if matches!(self.peek()?.ty, TokenType::Delimiter(Delimiter::LParen)) {
                    self.parse_call(name)
                } else {
                    Ok(Expr::Variable(name))
                }
            }

            TokenType::Delimiter(Delimiter::LParen) => {
                let expr = self.parse_expr()?;
                self.expect_delim(Delimiter::RParen)?;
                Ok(expr)
            }

            TokenType::Keyword(Keyword::If) => {
                let condition = self.parse_expr()?;
                let then_branch = match self.parse_scope()? {
                    Statement {
                        stmt: Stmt::Scope { statements },
                        ..
                    } => statements,
                    _ => unreachable!(),
                };

                let else_branch = if matches!(self.peek()?.ty, TokenType::Keyword(Keyword::Else)) {
                    self.advance()?;
                    match self.parse_scope()? {
                        Statement {
                            stmt: Stmt::Scope { statements },
                            ..
                        } => Some(statements),
                        _ => unreachable!(),
                    }
                } else {
                    None
                };

                Ok(Expr::If {
                    condition: Box::new(condition),
                    then_branch,
                    else_branch,
                })
            }

            t => anyhow::bail!("Unexpected token in expression: {:?}", t),
        }
    }

    fn parse_call(&mut self, name: String) -> anyhow::Result<Expr> {
        self.expect_delim(Delimiter::LParen)?;
        let mut args = Vec::new();

        if !matches!(self.peek()?.ty, TokenType::Delimiter(Delimiter::RParen)) {
            loop {
                args.push(self.parse_expr()?);
                if matches!(self.peek()?.ty, TokenType::Delimiter(Delimiter::Comma)) {
                    self.advance()?;
                } else {
                    break;
                }
            }
        }

        self.expect_delim(Delimiter::RParen)?;
        Ok(Expr::FunctionCall {
            name,
            arguments: args,
        })
    }

    fn parse_stmt(&mut self) -> anyhow::Result<Stmt> {
        match self.peek()?.ty {
            TokenType::Keyword(Keyword::Let) => self.parse_let(),
            TokenType::Keyword(Keyword::Func) => self.parse_function(),
            TokenType::Keyword(Keyword::While) => self.parse_while(),
            TokenType::Keyword(Keyword::Return) => self.parse_return(),
            TokenType::Delimiter(Delimiter::LBrace) => self.parse_scope().map(|x| x.stmt),
            TokenType::Keyword(Keyword::Break) => {
                self.advance()?;
                self.expect_delim(Delimiter::Semicolon)?;
                Ok(Stmt::Break)
            }
            TokenType::Keyword(Keyword::Continue) => {
                self.advance()?;
                self.expect_delim(Delimiter::Semicolon)?;
                Ok(Stmt::Continue)
            }
            TokenType::Keyword(Keyword::Extern) => {
                self.advance()?; // extern

                let name = match self.advance()?.ty.clone() {
                    TokenType::Identifier(name) => name,
                    t => anyhow::bail!("Expected function name, found {:?}", t),
                };

                self.expect_delim(Delimiter::LParen)?;

                let mut params = Vec::new();
                if !matches!(self.peek()?.ty, TokenType::Delimiter(Delimiter::RParen)) {
                    loop {
                        params.push(self.parse_param()?);
                        if matches!(self.peek()?.ty, TokenType::Delimiter(Delimiter::Comma)) {
                            self.advance()?;
                        } else {
                            break;
                        }
                    }
                }

                self.expect_delim(Delimiter::RParen)?;

                let ty = if matches!(self.peek()?.ty, TokenType::Delimiter(Delimiter::Arrow)) {
                    self.advance()?;
                    self.parse_type()?
                } else {
                    Type::Void
                };

                self.expect_delim(Delimiter::Semicolon)?;

                Ok(Stmt::Extern { name, params, ty })
            }
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_scope(&mut self) -> anyhow::Result<Statement> {
        self.expect_delim(Delimiter::LBrace)?;
        let location = self.peek()?.location;
        let mut statements = Vec::new();

        while !matches!(self.peek()?.ty, TokenType::Delimiter(Delimiter::RBrace)) {
            let location = self.peek()?.location;
            statements.push(Statement {
                stmt: self.parse_stmt()?,
                location,
            });
        }

        self.expect_delim(Delimiter::RBrace)?;
        Ok(Statement {
            stmt: Stmt::Scope { statements },
            location,
        })
    }

    fn parse_while(&mut self) -> anyhow::Result<Stmt> {
        self.advance()?; // while
        let condition = self.parse_expr()?;
        let body = Box::new(self.parse_scope()?);
        Ok(Stmt::While { condition, body })
    }

    fn parse_return(&mut self) -> anyhow::Result<Stmt> {
        self.advance()?; // return

        if matches!(self.peek()?.ty, TokenType::Delimiter(Delimiter::Semicolon)) {
            self.advance()?;
            Ok(Stmt::Return { value: None })
        } else {
            let value = self.parse_expr()?;
            self.expect_delim(Delimiter::Semicolon)?;
            Ok(Stmt::Return { value: Some(value) })
        }
    }

    fn parse_type(&mut self) -> anyhow::Result<Type> {
        match self.advance()?.ty.clone() {
            TokenType::Operator(Operator::Asterisk) => {
                Ok(Type::Pointer(Box::new(self.parse_type()?)))
            }
            TokenType::Identifier(name) => Ok(match name.as_str() {
                "i8" => Type::Numeric(NumericType::I8),
                "i16" => Type::Numeric(NumericType::I16),
                "i32" => Type::Numeric(NumericType::I32),
                "i64" => Type::Numeric(NumericType::I64),
                "is" => Type::Numeric(NumericType::ISize),
                "u8" => Type::Numeric(NumericType::U8),
                "u16" => Type::Numeric(NumericType::U16),
                "u32" => Type::Numeric(NumericType::U32),
                "u64" => Type::Numeric(NumericType::U64),
                "us" => Type::Numeric(NumericType::USize),
                "f32" => Type::Numeric(NumericType::F32),
                "f64" => Type::Numeric(NumericType::F64),
                "bool" => Type::Boolean,
                _ => Type::Named(name),
            }),
            TokenType::Delimiter(Delimiter::Variadic) => Ok(Type::Variadic),
            t => anyhow::bail!("Expected type, found {:?}", t),
        }
    }

    fn parse_let(&mut self) -> anyhow::Result<Stmt> {
        self.advance()?; // let

        let name = match self.advance()?.ty.clone() {
            TokenType::Identifier(name) => name,
            t => anyhow::bail!("Expected identifier after let, found {:?}", t),
        };

        let ty = if matches!(self.peek()?.ty, TokenType::Delimiter(Delimiter::Colon)) {
            self.advance()?;
            Some(self.parse_type()?)
        } else {
            None
        };

        anyhow::ensure!(
            matches!(self.peek()?.ty, TokenType::Operator(Operator::Assign)),
            "Expected '=' in let declaration"
        );
        self.advance()?;

        let value = self.parse_expr()?;
        self.expect_delim(Delimiter::Semicolon)?;

        Ok(Stmt::Let { name, ty, value })
    }

    fn parse_param(&mut self) -> anyhow::Result<Param> {
        let ty = self.parse_type()?;

        let name = match self.advance()?.ty.clone() {
            TokenType::Identifier(name) => name,
            t => anyhow::bail!("Expected parameter name, found {:?}", t),
        };

        Ok(Param { name, ty })
    }

    fn parse_function(&mut self) -> anyhow::Result<Stmt> {
        self.advance()?; // func

        let name = match self.advance()?.ty.clone() {
            TokenType::Identifier(name) => name,
            t => anyhow::bail!("Expected function name, found {:?}", t),
        };

        self.expect_delim(Delimiter::LParen)?;

        let mut params = Vec::new();
        if !matches!(self.peek()?.ty, TokenType::Delimiter(Delimiter::RParen)) {
            loop {
                params.push(self.parse_param()?);
                if matches!(self.peek()?.ty, TokenType::Delimiter(Delimiter::Comma)) {
                    self.advance()?;
                } else {
                    break;
                }
            }
        }

        self.expect_delim(Delimiter::RParen)?;

        let ty = if matches!(self.peek()?.ty, TokenType::Delimiter(Delimiter::Arrow)) {
            self.advance()?;
            self.parse_type()?
        } else {
            Type::Void
        };

        let body = Box::new(self.parse_scope()?);

        Ok(Stmt::Func {
            name,
            params,
            ty,
            body,
        })
    }

    pub fn parse(&mut self) -> anyhow::Result<()> {
        while !self.is_at_end() {
            let location = self.peek()?.location;
            let stmt = self.parse_stmt()?;
            self.global_scope.push(Statement { stmt, location });
        }

        Ok(())
    }
}
