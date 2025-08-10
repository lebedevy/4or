use std::collections::HashMap;

use crate::parser::{Expression, Statement, statement::Block};

struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
}

impl Resolver {
    fn resolve_statement(&mut self, statement: Statement) {
        match statement {
            Statement::Expression(expression) => todo!(),
            Statement::Variable(token, expression) => todo!(), //self.variable(),
            Statement::Block(block) => self.block(block),
            Statement::Function(ident, tokens, block) => todo!(),
            Statement::If(expression, statement, statement1) => todo!(),
            Statement::While(expression, statement) => todo!(),
            Statement::Return(token, statement) => todo!(),
        }
    }

    /// Block statement: introduces new scope
    fn block(&mut self, block: Block) {
        self.begin_scope();
        for statement in block.statements {
            self.resolve_statement(statement);
        }
        self.end_scope();
    }
    /// function declaration: introduces new scope for its body; binds parameters to that scope
    /// variable declaration: add new variable to current scope
    /// variable and assignment expressions: need to resolve their variables
    /// other nodes: must be traversed to find nested instances of the above

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn resolve_expression(expression: Expression) {}

    fn end_scope(&mut self) {
        self.scopes.pop();
    }
}
