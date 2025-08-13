use std::collections::HashMap;

use crate::parser::{
    Expression, Statement,
    statement::{Block, Identifier},
};

pub(crate) struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
}

// TODO: Once we resolve all the statements, we'll need to check if any variables are resolved

impl Resolver {
    pub(crate) fn new() -> Self {
        // this is the global scope
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    pub(crate) fn resolve(&mut self, statements: &Vec<Statement>) {
        for statement in statements {
            self.resolve_statement(statement);
        }
    }

    fn resolve_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Expression(expression) => self.resolve_expression(expression),
            Statement::Variable(ident, expression) => self.variable_statement(ident, expression),
            Statement::Block(block) => self.block(block),
            Statement::Function(ident, parameters, body) => self.function(ident, parameters, body),
            Statement::If(condition, then_branch, else_branch) => {
                self.resolve_expression(condition);
                self.resolve_statement(then_branch);
                if let Some(else_branch) = else_branch {
                    self.resolve_statement(else_branch);
                }
            }
            Statement::While(condition, statement) => {
                self.resolve_expression(condition);
                self.resolve_statement(statement);
            }
            Statement::Return(token, statement) => self.resolve_statement(statement),
        }
    }

    /// Block statement: introduces new scope
    fn block(&mut self, block: &Block) {
        self.begin_scope();
        for statement in block.statements.iter() {
            self.resolve_statement(statement);
        }
        self.end_scope();
    }
    /// function declaration: introduces new scope for its body; binds parameters to that scope
    fn function(&mut self, ident: &Identifier, parameters: &Vec<Identifier>, body: &Block) {
        self.declare(&ident.ident);
        self.define(&ident.ident);

        // TODO: This might have to be split to resolve class methods later
        self.begin_scope();
        for ident in parameters {
            self.declare(&ident.ident);
            self.define(&ident.ident);
        }
        self.block(body);
        self.end_scope();
    }

    /// variable declaration: add new variable to current scope
    fn variable_statement(&mut self, ident: &Identifier, expression: &Option<Expression>) {
        self.declare(&ident.ident);
        if let Some(expression) = expression {
            self.resolve_expression(expression);
            self.define(&ident.ident);
        }
    }
    /// variable and assignment expressions: need to resolve their variables
    /// other nodes: must be traversed to find nested instances of the above

    fn resolve_expression(&mut self, expression: &Expression) {
        match expression {
            Expression::Binary(left, _token, right) => {
                self.resolve_expression(left);
                self.resolve_expression(right);
            }
            Expression::Grouping(expression) => self.resolve_expression(expression),
            Expression::Literal(_literal) => (),
            Expression::Variable(ident) => self.resolve_local(&ident.ident),
            Expression::Unary(_token, expression) => self.resolve_expression(expression),
            Expression::Assignment(ident, expression) => {
                self.assignment_expression(ident, expression)
            }
            Expression::Logical(left, _token, right) => {
                self.resolve_expression(left);
                self.resolve_expression(right);
            }
            Expression::Call(callee, arguments) => {
                self.resolve_expression(callee);
                for argument in arguments {
                    self.resolve_expression(argument);
                }
            }
        }
    }

    fn assignment_expression(&mut self, ident: &Identifier, expression: &Box<Expression>) {
        self.resolve_expression(expression);
        self.resolve_local(&ident.ident);
    }

    // scope management
    fn resolve_local(&mut self, name: &str) {
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(name) {
                // TODO: Resolve in interpreter
                break;
            }
        }
    }

    fn declare(&mut self, name: &str) {
        self.scopes
            .last_mut()
            .expect("Missing scope")
            .insert(name.to_string(), false);
    }

    fn define(&mut self, name: &str) {
        let val = self
            .scopes
            .last_mut()
            .expect("Missing scope")
            .get_mut(name)
            .expect("Missing variable");

        *val = false;
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }
}
