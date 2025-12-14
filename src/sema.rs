use std::collections::HashMap;

use crate::{
    parser::{Expr, Expression, Statement, Stmt, Type},
    token::Location,
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
    ty: Option<Type>,
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
                self.build_expr(&Expression {
                    expr: value.clone(),
                    location: statement.location,
                })?;
                self.declare(Symbol {
                    name: name.clone(),
                    kind: SymbolKind::Variable,
                    ty: ty.clone(),
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
                        ty: Some(param.ty.clone()),
                        location: statement.location,
                    })?;
                }
                self.build_stmt(body)?;
                self.pop_scope();
            }
            Stmt::Scope { statements } => {
                self.push_scope();
                for statement in statements {
                    self.build_stmt(statement)?;
                }
                self.pop_scope();
            }
            Stmt::Expr(expr) => {
                self.build_expr(&Expression {
                    expr: expr.clone(),
                    location: statement.location,
                })?;
            }
            Stmt::While { condition, body } => {
                self.build_expr(&Expression {
                    expr: condition.clone(),
                    location: statement.location,
                })?;
                self.build_stmt(body)?;
            }
            Stmt::Return { value: Some(expr) } => {
                self.build_expr(&Expression {
                    expr: expr.clone(),
                    location: statement.location,
                })?;
            }
            Stmt::Return { value: None } | Stmt::Break | Stmt::Continue => {}
        }
        Ok(())
    }

    fn build_expr(&mut self, expression: &Expression) -> anyhow::Result<()> {
        match &expression.expr {
            Expr::Variable(name) => {
                anyhow::ensure!(
                    self.lookup(name).is_some(),
                    "Use of undeclared variable '{}' at {:?}",
                    name,
                    expression.location
                );
            }
            Expr::Binary { left, right, .. } => {
                self.build_expr(&Expression {
                    expr: *left.clone(),
                    location: expression.location,
                })?;
                self.build_expr(&Expression {
                    expr: *right.clone(),
                    location: expression.location,
                })?;
            }
            Expr::Unary { operand, .. } => {
                self.build_expr(&Expression {
                    expr: *operand.clone(),
                    location: expression.location,
                })?;
            }
            Expr::FunctionCall { name, arguments } => {
                anyhow::ensure!(
                    self.lookup(name).is_some(),
                    "Use of undeclared function '{}' at {:?}",
                    name,
                    expression.location
                );
                for arg in arguments {
                    self.build_expr(&Expression {
                        expr: arg.clone(),
                        location: expression.location,
                    })?;
                }
            }
            Expr::Assignment { target, value } => {
                self.build_expr(&Expression {
                    expr: *target.clone(),
                    location: expression.location,
                })?;
                self.build_expr(&Expression {
                    expr: *value.clone(),
                    location: expression.location,
                })?;
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.build_expr(&Expression {
                    expr: *condition.clone(),
                    location: expression.location,
                })?;
                for stmt in then_branch {
                    self.build_stmt(stmt)?;
                }
                if let Some(else_branch) = else_branch {
                    for stmt in else_branch {
                        self.build_stmt(stmt)?;
                    }
                }
            }
            Expr::Literal(_) => {}
        }
        Ok(())
    }

    pub fn build(&mut self, statements: &[Statement]) -> anyhow::Result<()> {
        self.declare(Symbol {
            name: "print".to_string(),
            kind: SymbolKind::Function,
            ty: None,
            location: Location::default(),
        })?;

        for stmt in statements {
            self.build_stmt(stmt)?;
        }
        Ok(())
    }
}
