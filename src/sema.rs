use std::collections::HashMap;

use crate::{
    parser::{Expr, Expression, Statement, Stmt, Type},
    token::{Literal, Location, NumericType, Operator},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Variable,
    Function,
    Parameter,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
    name: String,
    kind: SymbolKind,
    ty: Type,
    location: Location,
}

#[derive(Debug)]
pub struct Scope {
    symbols: HashMap<String, Symbol>,
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            symbols: HashMap::new(),
        }
    }

    fn insert(&mut self, sym: Symbol) -> anyhow::Result<()> {
        anyhow::ensure!(
            !self.symbols.contains_key(&sym.name),
            "{:?} '{}' redeclaration at {:?}",
            sym.kind,
            sym.name,
            sym.location
        );
        self.symbols.insert(sym.name.clone(), sym);
        Ok(())
    }

    fn get(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    scopes: Vec<Scope>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            scopes: vec![Scope::new()],
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, sym: Symbol) -> anyhow::Result<()> {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(sym)?;
        } else {
            anyhow::bail!("No scope found");
        }

        Ok(())
    }

    fn lookup(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(sym) = scope.get(name) {
                return Some(sym);
            }
        }
        None
    }

    fn build_stmt(&mut self, statement: &Statement) -> anyhow::Result<()> {
        match &statement.stmt {
            Stmt::Let { name, ty, value } => {
                self.build_expr(value, statement.location)?;
                let ty = if let Some(ty) = ty {
                    ty.clone()
                } else {
                    self.expr_type(value, statement.location)?
                };
                self.declare(Symbol {
                    name: name.clone(),
                    kind: SymbolKind::Variable,
                    ty,
                    location: statement.location,
                })?;
            }
            Stmt::Func {
                name,
                params,
                ty,
                body,
            } => {
                self.declare(Symbol {
                    name: name.clone(),
                    kind: SymbolKind::Function,
                    ty: ty.clone(),
                    location: statement.location,
                })?;
                self.push_scope();
                for param in params {
                    self.declare(Symbol {
                        name: param.name.clone(),
                        kind: SymbolKind::Parameter,
                        ty: param.ty.clone(),
                        location: statement.location,
                    })?;
                }
                self.build_stmt(body)?;
                self.pop_scope();
            }
            Stmt::Extern { name, ty, .. } => {
                self.declare(Symbol {
                    name: name.clone(),
                    kind: SymbolKind::Function,
                    ty: ty.clone(),
                    location: statement.location,
                })?;
            }
            Stmt::Scope { statements } => {
                self.push_scope();
                for statement in statements {
                    self.build_stmt(statement)?;
                }
                self.pop_scope();
            }
            Stmt::Expr(expr) => {
                self.build_expr(expr, statement.location)?;
            }
            Stmt::While { condition, body } => {
                self.build_expr(condition, statement.location)?;
                self.build_stmt(body)?;
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.build_expr(condition, statement.location)?;

                self.push_scope();
                for stmt in then_branch {
                    self.build_stmt(stmt)?;
                }
                self.pop_scope();

                if let Some(else_branch) = else_branch {
                    self.push_scope();
                    for stmt in else_branch {
                        self.build_stmt(stmt)?;
                    }
                    self.pop_scope();
                }
            }
            Stmt::Return { value: Some(expr) } => {
                self.build_expr(expr, statement.location)?;
            }
            Stmt::Return { value: None } | Stmt::Break | Stmt::Continue | Stmt::Semicolon => {}
        }
        Ok(())
    }

    fn build_expr(&mut self, expr: &Expr, location: Location) -> anyhow::Result<()> {
        match expr {
            Expr::Variable(name) => {
                anyhow::ensure!(
                    self.lookup(name).is_some(),
                    "Use of undeclared variable '{}' at {:?}",
                    name,
                    location
                );
            }
            Expr::Binary { left, right, .. } => {
                let lhs_type = self.expr_type(left, location)?;
                let rhs_type = self.expr_type(right, location)?;
                if lhs_type != rhs_type {
                    anyhow::bail!(
                        "Type mismatch in binary expression at {:?}: left is {:?}, right is {:?}",
                        location,
                        lhs_type,
                        rhs_type
                    );
                }
                self.build_expr(left, location)?;
                self.build_expr(right, location)?;
            }
            Expr::Unary { operand, .. } => {
                self.build_expr(operand, location)?;
            }
            Expr::FunctionCall { name, arguments } => {
                anyhow::ensure!(
                    self.lookup(name).is_some(),
                    "Use of undeclared function '{}' at {:?}",
                    name,
                    location
                );
                for arg in arguments {
                    self.build_expr(arg, location)?;
                }
            }
            Expr::Assignment { target, value } => {
                self.build_expr(target, location)?;
                self.build_expr(value, location)?;
            }
            Expr::Literal(_) => {}
        }
        Ok(())
    }

    fn expr_type(&self, expr: &Expr, location: Location) -> anyhow::Result<Type> {
        match expr {
            Expr::Literal(lit) => match lit {
                Literal::Numeric(literal) => Ok(Type::Numeric(
                    NumericType::from_literal(literal)?.unwrap_or(if literal.contains('.') {
                        NumericType::F64
                    } else {
                        NumericType::I32
                    }),
                )),
                Literal::Char(_) => Ok(Type::Named("char".to_string())),
                Literal::String(_) => Ok(Type::Named("String".to_string())),
                Literal::Boolean(_) => Ok(Type::Boolean),
            },
            Expr::Variable(name) => {
                let sym = self.lookup(name).ok_or_else(|| {
                    anyhow::anyhow!(
                        "Use of undeclared variable '{}' at {:?}",
                        name,
                        location
                    )
                })?;

                Ok(sym.ty.clone())
            }
            Expr::Binary {
                left,
                right,
                operator,
            } => {
                let left_type = self.expr_type(left, location)?;
                let right_type = self.expr_type(right, location)?;

                if left_type != right_type {
                    anyhow::bail!(
                        "Type mismatch in binary expression at {:?}: left is {:?}, right is {:?}",
                        location,
                        left_type,
                        right_type
                    );
                }

                match operator {
                    Operator::Exclem | Operator::LogicalAnd | Operator::LogicalOr => {
                        if left_type != Type::Boolean {
                            anyhow::bail!(
                                "Logical operators require boolean operands at {:?}, found {:?}",
                                location,
                                left_type
                            );
                        }
                        Ok(Type::Boolean)
                    }
                    Operator::Equals
                    | Operator::NotEquals
                    | Operator::Less
                    | Operator::LessEquals
                    | Operator::Greater
                    | Operator::GreaterEquals => Ok(Type::Boolean),
                    _ => Ok(left_type),
                }
            }
            Expr::Unary { operand, operator } => {
                let op_type = self.expr_type(operand, location)?;
                Ok(match operator {
                    Operator::Ampersand => Type::Pointer(Box::new(op_type)),
                    Operator::Asterisk => match op_type {
                        Type::Pointer(x) => *x,
                        _ => anyhow::bail!(
                            "Dereferencing a non-pointer type at {:?}",
                            location
                        ),
                    },
                    _ => op_type,
                })
            }
            Expr::Assignment { target, value } => {
                let target_type = self.expr_type(target, location)?;
                let value_type = self.expr_type(value, location)?;

                if target_type != value_type {
                    anyhow::bail!(
                        "Type mismatch in assignment at {:?}: target is {:?}, value is {:?}",
                        location,
                        target_type,
                        value_type
                    );
                }

                Ok(target_type)
            }
            Expr::FunctionCall { name, .. } => {
                let sym = self.lookup(name).ok_or_else(|| {
                    anyhow::anyhow!(
                        "Call to undeclared function '{}' at {:?}",
                        name,
                        location
                    )
                })?;

                Ok(sym.ty.clone())
            }
        }
    }

    fn block_type(&self, stmts: &[Statement]) -> anyhow::Result<Type> {
        let mut last_type = Type::Void;
        for stmt in stmts {
            last_type = self.stmt_type(stmt)?;
        }
        Ok(last_type)
    }

    fn stmt_type(&self, statement: &Statement) -> anyhow::Result<Type> {
        match &statement.stmt {
            Stmt::Let { ty, value, .. } => {
                let expr_type = self.expr_type(value, statement.location)?;
                if let Some(ty) = ty
                    && *ty != expr_type
                {
                    anyhow::bail!(
                        "Type mismatch in let statement at {:?}: declared as {:?}, assigned {:?}",
                        statement.location,
                        ty,
                        expr_type
                    );
                }
                Ok(Type::Void)
            }
            Stmt::Expr(expr) => self.expr_type(expr, statement.location),
            Stmt::Return { value: Some(expr) } => self.expr_type(expr, statement.location),
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_type = self.expr_type(condition, statement.location)?;
                if cond_type != Type::Boolean {
                    anyhow::bail!(
                        "Condition in if statement must be boolean at {:?}, found {:?}",
                        statement.location,
                        cond_type
                    );
                }

                let then_type = self.block_type(then_branch)?;

                let else_type = if let Some(else_branch) = else_branch {
                    Some(self.block_type(else_branch)?)
                } else {
                    None
                };

                if let Some(else_type) = else_type
                    && then_type != else_type
                {
                    anyhow::bail!(
                        "Type mismatch in if statement branches at {:?}: then is {:?}, else is {:?}",
                        statement.location,
                        then_type,
                        else_type
                    );
                }

                Ok(then_type)
            }
            Stmt::Scope { statements } => self.block_type(statements),
            Stmt::Func { ty, .. } => Ok(ty.clone()),
            Stmt::Extern { ty, .. } => Ok(ty.clone()),
            Stmt::While { body, .. } => {
                self.stmt_type(body)?;
                Ok(Type::Void)
            }
            Stmt::Return { value: None } | Stmt::Break | Stmt::Continue | Stmt::Semicolon => {
                Ok(Type::Void)
            }
        }
    }

    pub fn build(&mut self, statements: &[Statement]) -> anyhow::Result<()> {
        self.declare(Symbol {
            name: "print".to_string(),
            kind: SymbolKind::Function,
            ty: Type::Void,
            location: Location::default(),
        })?;
        self.declare(Symbol {
            name: "exit".to_string(),
            kind: SymbolKind::Function,
            ty: Type::Void,
            location: Location::default(),
        })?;

        for stmt in statements {
            self.build_stmt(stmt)?;
            self.stmt_type(stmt)?;
        }

        Ok(())
    }
}
