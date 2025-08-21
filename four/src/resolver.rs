use std::collections::HashMap;

use crate::{
    parser::{
        Expression, Statement,
        statement::{Block, Identifier},
    },
    token::Token,
};

pub struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
}

// TODO: Once we resolve all the statements, we'll need to check if any variables are resolved

impl Resolver {
    pub fn new() -> Self {
        Self {
            // this is the global scope
            scopes: vec![HashMap::new()],
        }
    }

    pub(crate) fn resolve(
        &mut self,
        statements: &Vec<Statement>,
        register_mapping: &mut impl FnMut(usize, usize),
    ) -> Result<(), ResolverError> {
        for statement in statements {
            self.resolve_statement(statement, register_mapping)?;
        }

        Ok(())
    }

    fn resolve_statement(
        &mut self,
        statement: &Statement,
        register_mapping: &mut impl FnMut(usize, usize),
    ) -> Result<(), ResolverError> {
        match statement {
            Statement::Expression(expression) => {
                self.resolve_expression(expression, register_mapping)
            }
            Statement::Variable(ident, expression) => {
                self.variable_statement(ident, expression, register_mapping)
            }
            Statement::Block(block) => self.block(block, register_mapping),
            Statement::Function(ident, parameters, body) => {
                self.function(ident, parameters, body, register_mapping)
            }
            Statement::If(condition, then_branch, else_branch) => {
                self.resolve_expression(condition, register_mapping)?;
                self.resolve_statement(then_branch, register_mapping)?;
                if let Some(else_branch) = else_branch {
                    self.resolve_statement(else_branch, register_mapping)?;
                }
                Ok(())
            }
            Statement::While(condition, statement) => {
                self.resolve_expression(condition, register_mapping)?;
                self.resolve_statement(statement, register_mapping)
            }
            Statement::Return(token, statement) => {
                self.return_statement(token, statement, register_mapping)
            }
        }
    }

    /// Block statement: introduces new scope
    fn block(
        &mut self,
        block: &Block,
        register_mapping: &mut impl FnMut(usize, usize),
    ) -> Result<(), ResolverError> {
        self.begin_scope();
        for statement in block.statements.iter() {
            self.resolve_statement(statement, register_mapping)?;
        }
        self.end_scope();

        Ok(())
    }

    /// function declaration: introduces new scope for its body; binds parameters to that scope
    fn function(
        &mut self,
        ident: &Identifier,
        parameters: &Vec<Identifier>,
        body: &Block,
        register_mapping: &mut impl FnMut(usize, usize),
    ) -> Result<(), ResolverError> {
        self.declare(&ident.ident);
        self.define(&ident)?;

        // TODO: This might have to be split to resolve class methods later
        self.begin_scope();
        for ident in parameters {
            self.declare(&ident.ident);
            self.define(&ident)?;
            // This is necessary for native functions, since we don't explicitly resolve the body
            // TODO: investigate further
            self.resolve_local(ident, register_mapping)?;
        }
        for statement in &body.statements {
            self.resolve_statement(statement, register_mapping)?;
        }
        self.end_scope();

        Ok(())
    }

    /// variable declaration: add new variable to current scope
    fn variable_statement(
        &mut self,
        ident: &Identifier,
        expression: &Option<Expression>,
        register_mapping: &mut impl FnMut(usize, usize),
    ) -> Result<(), ResolverError> {
        self.declare(&ident.ident);
        if let Some(expression) = expression {
            self.resolve_expression(expression, register_mapping)?;
            self.define(&ident)?;
        }
        Ok(())
    }

    fn return_statement(
        &mut self,
        token: &Token,
        statement: &Statement,
        register_mapping: &mut impl FnMut(usize, usize),
    ) -> Result<(), ResolverError> {
        // Attempting to return from the global scope
        if self.scopes.len() == 0 {
            return Err(ResolverError::Error(
                token.clone(),
                "Attempting to return from the global scope".into(),
            ));
        }

        self.resolve_statement(statement, register_mapping)
    }

    /// variable and assignment expressions: need to resolve their variables
    /// other nodes: must be traversed to find nested instances of the above
    fn resolve_expression(
        &mut self,
        expression: &Expression,
        register_mapping: &mut impl FnMut(usize, usize),
    ) -> Result<(), ResolverError> {
        match expression {
            Expression::Binary(left, _token, right) => {
                self.resolve_expression(left, register_mapping)?;
                self.resolve_expression(right, register_mapping)
            }
            Expression::Grouping(expression) => {
                self.resolve_expression(expression, register_mapping)
            }
            Expression::Literal(_literal) => Ok(()),
            Expression::Variable(ident) => self.resolve_local(&ident, register_mapping),
            Expression::Unary(_token, expression) => {
                self.resolve_expression(expression, register_mapping)
            }
            Expression::Assignment(ident, expression) => {
                self.assignment_expression(ident, expression, register_mapping)
            }
            Expression::Logical(left, _token, right) => {
                self.resolve_expression(left, register_mapping)?;
                self.resolve_expression(right, register_mapping)
            }
            Expression::Call(callee, arguments) => {
                self.resolve_expression(callee, register_mapping)?;
                for argument in arguments {
                    self.resolve_expression(argument, register_mapping)?;
                }
                Ok(())
            }
        }
    }

    fn assignment_expression(
        &mut self,
        ident: &Identifier,
        expression: &Box<Expression>,
        register_mapping: &mut impl FnMut(usize, usize) -> (),
    ) -> Result<(), ResolverError> {
        self.resolve_expression(expression, register_mapping)?;
        self.resolve_local(&ident, register_mapping)
    }

    // scope management
    fn resolve_local(
        &mut self,
        ident: &Identifier,
        register_mapping: &mut impl FnMut(usize, usize) -> (),
    ) -> Result<(), ResolverError> {
        for (depth, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(&ident.ident) {
                register_mapping(ident.global_id, depth);
                return Ok(());
            }
        }

        return Err(ResolverError::Error(
            ident.token.clone(),
            format!("Unknown identifier '{}'", ident.ident),
        ));
    }

    fn declare(&mut self, name: &str) {
        self.scopes
            .last_mut()
            .expect("Missing scope")
            .insert(name.to_string(), false);
    }

    fn define(&mut self, ident: &Identifier) -> Result<(), ResolverError> {
        match self
            .scopes
            .last_mut()
            .expect("Missing scope")
            .get_mut(&ident.ident)
        {
            Some(val) => {
                *val = false;
                Ok(())
            }
            None => Err(ResolverError::Error(
                ident.token.clone(),
                format!(
                    "Attempting to assign to undeclared variable '{}'",
                    ident.ident
                ),
            )),
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }
}

pub(super) enum ResolverError {
    Error(Token, String),
}

#[cfg(test)]
mod tests {
    use std::ops::Index;

    use crate::{
        Parser, ParserError, ProgramError,
        parser::{Expression, Statement},
        resolver::Resolver,
        scanner::Scanner,
    };

    fn get_program_from_string(string: &str) -> Result<Vec<Statement>, ParserError> {
        let mut scanner = Scanner::new(string.to_string());
        let tokens = scanner.scan_tokens();
        Ok(Parser::parse(tokens.into_iter().peekable())?)
    }

    #[test]
    fn tracks_declarations() -> Result<(), ProgramError> {
        let program = "let a = 1; let b = a;";
        let program = get_program_from_string(program)?;
        let mut resolver = Resolver::new();

        resolver.resolve(&program, &mut |_id, _distance| {})?;

        assert!(resolver.scopes.first().unwrap().contains_key("a"));
        assert!(resolver.scopes.first().unwrap().contains_key("b"));

        Ok(())
    }

    #[test]
    fn resolves_declaration() -> Result<(), ProgramError> {
        let program = "let a = 1; let b = a;";
        let program = get_program_from_string(program)?;
        let mut resolver = Resolver::new();

        let assignment_ident = match program.index(1) {
            Statement::Variable(_, Some(Expression::Variable(ident))) => ident,
            _ => panic!("Generated unexpected statements"),
        };

        let mut id = 0;
        let mut called = false;

        resolver.resolve(&program, &mut |var_id, distance| {
            println!("Mapping: {} {}", id, distance);
            id = var_id;
            called = true;
        })?;

        assert!(called, "Did not map variable");
        assert_eq!(
            assignment_ident.ident, "a",
            "Did not find correct assigment variable"
        );
        assert_eq!(
            assignment_ident.global_id, id,
            "Did not map to the correct variable"
        );

        Ok(())
    }

    #[test]
    fn resolves_function() -> Result<(), ProgramError> {
        let program = "fn test() {} test();";
        let program = get_program_from_string(program)?;
        let mut resolver = Resolver::new();

        let assignment_ident = match program.index(1) {
            Statement::Expression(Expression::Call(item, _)) => match item.as_ref() {
                Expression::Variable(ident) => ident,
                _ => panic!("Generated unexpected statements"),
            },
            _ => panic!("Generated unexpected statements"),
        };

        let mut id = 0;
        let mut called = false;

        resolver.resolve(&program, &mut |var_id, distance| {
            println!("Mapping: {} {}", id, distance);
            id = var_id;
            called = true;
        })?;

        assert!(called, "Did not map variable");
        assert_eq!(
            assignment_ident.ident, "test",
            "Did not find correct assigment variable"
        );
        assert_eq!(
            assignment_ident.global_id, id,
            "Did not map to the correct variable"
        );

        Ok(())
    }

    #[test]
    fn resolves_function_params() -> Result<(), ProgramError> {
        let program = "fn test(a) {} test(1);";
        let program = get_program_from_string(program)?;
        let mut resolver = Resolver::new();

        let assignment_ident = match program.index(1) {
            Statement::Expression(Expression::Call(item, _)) => match item.as_ref() {
                Expression::Variable(ident) => ident,
                _ => panic!("Generated unexpected statements"),
            },
            _ => panic!("Generated unexpected statements"),
        };

        let mut id = 0;
        let mut called = false;

        resolver.resolve(&program, &mut |var_id, _| {
            id = var_id;
            called = true;
        })?;

        assert!(called, "Did not map variable");
        assert_eq!(
            assignment_ident.ident, "test",
            "Did not find correct assigment variable"
        );
        assert_eq!(
            assignment_ident.global_id, id,
            "Did not map to the correct variable"
        );

        Ok(())
    }

    #[test]
    fn resolves_nested_call() -> Result<(), ProgramError> {
        let program = "
fn test() {}
fn test_2() { test(); }
";
        let program = get_program_from_string(program)?;
        let mut resolver = Resolver::new();

        let Some(assignment_ident) = (match program.index(1) {
            Statement::Function(_, _, block) => match block.statements.first() {
                Some(Statement::Expression(Expression::Call(ident, _))) => match ident.as_ref() {
                    Expression::Variable(ident) => Some(ident),
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        }) else {
            panic!("Generated unexpected statements");
        };

        let mut distance = 0;
        let mut called = false;

        resolver.resolve(&program, &mut |_, var_distance| {
            distance = var_distance;
            called = true;
        })?;

        assert!(called, "Did not map variable");
        assert_eq!(
            assignment_ident.ident, "test",
            "Did not find correct assigment variable"
        );
        assert_eq!(distance, 1, "Did not map to the correct scope");

        Ok(())
    }
}
