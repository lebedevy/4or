use std::{fmt::Display, sync::Arc};

use crate::{
    environment::{Environment, EnvironmentError},
    interpreter::{
        function::UserFn,
        types::{ReferenceTypes, Types},
    },
    parser::{Expression, Literal, Statement},
    token::{Token, TokenType},
};

pub(super) mod function;
pub(crate) mod types;

struct EnvironmentWrapper {
    environment: Option<Environment>,
}

impl EnvironmentWrapper {
    fn new() -> Self {
        Self {
            environment: Some(Environment::new()),
        }
    }

    /// Create a new scope that has closure over the current scope
    /// The new scope will be set as the current scope
    fn create_scope(&mut self) -> Result<(), InterpreterError> {
        // Move the current scope out of the current reference
        let current = match self.environment.take() {
            Some(scope) => scope,
            None => {
                return Err(InterpreterError::EnviornmentError(
                    "Missing current scope".to_owned(),
                ));
            }
        };

        self.environment = Some(Environment::create_scope(current));

        Ok(())
    }

    fn pop_scope(&mut self) -> Result<(), InterpreterError> {
        // Move the current scope out of the current reference
        let current = match self.environment.take() {
            Some(scope) => scope,
            None => {
                return Err(InterpreterError::EnviornmentError(
                    "Missing current scope".to_owned(),
                ));
            }
        };

        let closure = match current.close() {
            Some(closure) => closure,
            None => {
                return Err(InterpreterError::EnviornmentError(
                    "Missing closure inside of environment".to_owned(),
                ));
            }
        };

        self.environment = Some(*closure);

        Ok(())
    }

    fn define(&mut self, name: &str, value: Types) -> Result<(), InterpreterError> {
        match &mut self.environment {
            Some(scope) => Ok(scope.define(name, value)?),
            None => {
                return Err(InterpreterError::EnviornmentError(
                    "Missing current scope".to_owned(),
                ));
            }
        }
    }

    fn assign(&mut self, name: &str, value: Types) -> Result<(), InterpreterError> {
        match &mut self.environment {
            Some(scope) => Ok(scope.assign(name, value)?),
            None => {
                return Err(InterpreterError::EnviornmentError(
                    "Missing current scope".to_owned(),
                ));
            }
        }
    }

    fn get(&self, name: &str) -> Result<Types, InterpreterError> {
        match &self.environment {
            Some(scope) => Ok(scope.get(name)?),
            None => {
                return Err(InterpreterError::EnviornmentError(
                    "Missing current scope".to_owned(),
                ));
            }
        }
    }
}

pub struct Interpreter {
    environment: EnvironmentWrapper,
}

enum Return {
    Expression(Types),
    Return(Types),
    None,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            environment: EnvironmentWrapper::new(),
        }
    }

    pub(super) fn run(&mut self, statements: Vec<Statement>) -> Result<(), InterpreterError> {
        for statement in statements {
            self.execute(&statement)?;
        }

        Ok(())
    }

    fn execute(&mut self, statement: &Statement) -> Result<Return, InterpreterError> {
        match statement {
            Statement::Expression(expression) => {
                return Ok(Return::Expression(self.evaluate(&expression)?));
            }
            Statement::Variable(token, expression) => {
                let value = match expression {
                    Some(expression) => self.evaluate(&expression)?,
                    None => Types::Primitive(Literal::Nil),
                };

                let identifier = match &token.token_type {
                    TokenType::Identifier(iden) => iden,
                    _ => return Err(InterpreterError::InvalidVariableIdentifier(token.clone())),
                };

                self.environment.define(identifier, value)?;
            }
            Statement::Block(statements) => {
                self.environment.create_scope()?;

                for statement in statements {
                    if let Return::Return(r_val) = self.execute(statement)? {
                        self.environment.pop_scope()?;
                        return Ok(Return::Return(r_val));
                    }
                }

                self.environment.pop_scope()?;
            }
            Statement::If(expression, statement, else_statement) => {
                let val = match self.evaluate(&expression)? {
                    Types::Primitive(Literal::Bool(bool)) => bool,
                    val => {
                        return Err(InterpreterError::ExpectedBool(val));
                    }
                };

                let r_val = if val {
                    self.execute(statement)?
                } else if let Some(statement) = else_statement {
                    self.execute(statement)?
                } else {
                    Return::None
                };

                if let Return::Return(r_val) = r_val {
                    return Ok(Return::Return(r_val));
                }
            }
            Statement::While(expression, statement) => loop {
                match self.evaluate(&expression)? {
                    Types::Primitive(Literal::Bool(condition)) => {
                        if !condition {
                            break;
                        }
                    }
                    condition => return Err(InterpreterError::ExpectedBool(condition)),
                }

                if let Return::Return(r_val) = self.execute(statement)? {
                    return Ok(Return::Return(r_val));
                }
            },
            Statement::Function(name, params, body) => {
                // fn declaration
                let name = match &name.token_type {
                    TokenType::Identifier(name) => name,
                    _ => return Err(InterpreterError::InvalidVariableIdentifier(name.clone())),
                };

                let params = params.iter().map(|x| x.clone()).collect();

                self.environment.define(
                    name.as_str(),
                    Types::Reference(ReferenceTypes::Function(Arc::new(UserFn::new(
                        name.as_str(),
                        params,
                        body.clone(),
                    )))),
                )?;
            }
            Statement::Return(_token, statement) => {
                return Ok(Return::Return(match self.execute(statement)? {
                    Return::Expression(val) | Return::Return(val) => val,
                    Return::None => Types::Primitive(Literal::Nil),
                }));
            }
        };

        Ok(Return::None)
    }
}

#[derive(Debug)]
pub(super) enum InterpreterError {
    InvalidUnary(Token),
    InvalidBinary(Token),
    InvalidLogicalOperator(Token),
    InvalidVariableIdentifier(Token),
    UndefinedVariable(String),
    ExpectedBool(Types),
    NotAFunction(Types),
    InvalidFunctionBody(String),
    InvalidFunctionCall(String),
    EnviornmentError(String),
}

impl Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InterpreterError::InvalidUnary(token) => {
                write!(f, "Interpreter error: Invalid unary - {}", token)?;
            }
            InterpreterError::InvalidBinary(token) => {
                write!(f, "Interpreter error: Invalid binary - {}", token)?;
            }
            InterpreterError::InvalidVariableIdentifier(token) => {
                write!(
                    f,
                    "Interpreter error: Invalid variable identifier - {}",
                    token
                )?;
            }
            InterpreterError::UndefinedVariable(name) => {
                write!(f, "Interpreter error: Undefined variable - {}", name)?;
            }
            InterpreterError::ExpectedBool(literal) => {
                write!(f, "Interpreter error: Expected bool, got '{}'", literal)?
            }
            InterpreterError::InvalidLogicalOperator(token) => write!(
                f,
                "Interpreter error: Expected 'and' or 'or', got '{}'",
                token
            )?,
            InterpreterError::NotAFunction(types) => {
                write!(f, "Attempting to call a non-function object {}", types)?
            }
            InterpreterError::InvalidFunctionBody(name) => {
                write!(f, "Invalid function body for function '{}'", name)?
            }
            InterpreterError::InvalidFunctionCall(err) => write!(f, "{}", err)?,
            InterpreterError::EnviornmentError(err) => write!(f, "{}", err)?,
        };

        Ok(())
    }
}

impl Interpreter {
    fn evaluate(&mut self, expression: &Expression) -> Result<Types, InterpreterError> {
        match expression {
            Expression::Binary(left, token, right) => self.binary(left, token, right),
            Expression::Grouping(expression) => self.grouping(expression),
            // TODO: consider implications of cloning
            Expression::Literal(literal) => Ok(Types::Primitive(literal.clone())),
            Expression::Unary(token, expression) => self.unary(token, expression),
            Expression::Variable(token) => self.variable(token),
            Expression::Assignment(token, expression) => self.assign(token, expression),
            Expression::Logical(left, operator, right) => self.logical(left, operator, right),
            Expression::Call(expression, args) => self.call_function(expression, args),
        }
    }

    fn call_function(
        &mut self,
        expression: &Expression,
        args: &Vec<Expression>,
    ) -> Result<Types, InterpreterError> {
        let callee = self.evaluate(expression)?;

        let callee = match callee {
            Types::Reference(ReferenceTypes::Function(func)) => func,
            _ => return Err(InterpreterError::NotAFunction(callee)),
        };

        // Execute body with its own scope
        self.environment.create_scope()?;

        let params = callee.get_params();

        if params.len() != args.len() {
            return Err(InterpreterError::InvalidFunctionCall(
                "Invalid arg count".to_string(),
            ));
        }

        for (param, arg) in params.iter().zip(args) {
            let param = match &param.token_type {
                TokenType::Identifier(iden) => iden,
                _ => {
                    return Err(InterpreterError::InvalidFunctionCall(
                        "invalid param defintion".to_string(),
                    ));
                }
            };
            let val = self.evaluate(arg)?;
            self.environment.define(param.as_str(), val)?;
        }

        callee.call(self)?;

        self.environment.pop_scope()?;

        Ok(Types::Primitive(Literal::Nil))
    }

    fn logical(
        &mut self,
        left: &Expression,
        operator: &Token,
        right: &Expression,
    ) -> Result<Types, InterpreterError> {
        let left = match self.evaluate(left)? {
            Types::Primitive(Literal::Bool(val)) => val,
            left => return Err(InterpreterError::ExpectedBool(left)),
        };

        // if and and false, return false; if or and true, return true
        match &operator.token_type {
            TokenType::And => {
                if !left {
                    return Ok(Types::Primitive(Literal::Bool(left)));
                }
            }
            TokenType::Or => {
                if left {
                    return Ok(Types::Primitive(Literal::Bool(left)));
                }
            }
            _ => return Err(InterpreterError::InvalidLogicalOperator(operator.clone())),
        };

        Ok(self.evaluate(right)?)
    }

    fn variable(&mut self, token: &Token) -> Result<Types, InterpreterError> {
        match &token.token_type {
            TokenType::Identifier(iden) => Ok(self.environment.get(iden)?),
            _ => return Err(InterpreterError::InvalidVariableIdentifier(token.clone())),
        }
    }

    fn assign(
        &mut self,
        token: &Token,
        expression: &Expression,
    ) -> Result<Types, InterpreterError> {
        let value = self.evaluate(expression)?;

        match &token.token_type {
            TokenType::Identifier(iden) => self.environment.assign(iden, value.clone())?,
            _ => return Err(InterpreterError::InvalidVariableIdentifier(token.clone())),
        };

        Ok(value)
    }

    fn binary(
        &mut self,
        left: &Expression,
        operator: &Token,
        right: &Expression,
    ) -> Result<Types, InterpreterError> {
        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;

        let (left, right) = match (left, right) {
            (Types::Primitive(left), Types::Primitive(right)) => (left, right),
            _ => return Err(InterpreterError::InvalidBinary(operator.clone())),
        };

        // TODO: int overflow
        let res = match (&operator.token_type, left, right) {
            // Numeric
            (TokenType::Minus, Literal::Number(left), Literal::Number(right)) => {
                Literal::Number(left - right)
            }
            (TokenType::Plus, Literal::Number(left), Literal::Number(right)) => {
                Literal::Number(left + right)
            }
            (TokenType::Slash, Literal::Number(left), Literal::Number(right)) => {
                Literal::Number(left / right)
            }
            (TokenType::Star, Literal::Number(left), Literal::Number(right)) => {
                Literal::Number(left * right)
            }
            // Strings
            (TokenType::Plus, Literal::String(left), Literal::String(right)) => {
                Literal::String(left + &right)
            }
            // Comparison
            (TokenType::Greater, Literal::Number(left), Literal::Number(right)) => {
                Literal::Bool(left > right)
            }
            (TokenType::GreaterEqual, Literal::Number(left), Literal::Number(right)) => {
                Literal::Bool(left >= right)
            }
            (TokenType::Less, Literal::Number(left), Literal::Number(right)) => {
                Literal::Bool(left < right)
            }
            (TokenType::LessEqual, Literal::Number(left), Literal::Number(right)) => {
                Literal::Bool(left <= right)
            }
            // Equality
            // TODO: Consider the implications; currently just outsourcing the comparison to rust
            (TokenType::EqualEqual, right, left) => Literal::Bool(right == left),
            (TokenType::BangEqual, right, left) => Literal::Bool(right != left),
            _val => return Err(InterpreterError::InvalidBinary(operator.clone())),
        };

        Ok(Types::Primitive(res))
    }

    fn unary(&mut self, token: &Token, expression: &Expression) -> Result<Types, InterpreterError> {
        let right = self.evaluate(expression)?;

        let res = match (&token.token_type, right) {
            (TokenType::Minus, Types::Primitive(Literal::Number(num))) => Literal::Number(-num),
            // We are not allowing the concept of "truthiness"; give me bool or get bonked
            (TokenType::Bang, Types::Primitive(Literal::Bool(val))) => Literal::Bool(!val),
            _val => return Err(InterpreterError::InvalidUnary(token.clone())),
        };

        Ok(Types::Primitive(res))
    }

    fn grouping(&mut self, expression: &Expression) -> Result<Types, InterpreterError> {
        self.evaluate(expression)
    }

    fn get_variable(&mut self, name: &str) -> Result<Types, InterpreterError> {
        Ok(self.environment.get(name)?)
    }
}

impl From<EnvironmentError> for InterpreterError {
    fn from(value: EnvironmentError) -> Self {
        match value {
            EnvironmentError::UndefinedVariable(value) => {
                InterpreterError::UndefinedVariable(value)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        interpreter::types::Types,
        parser::{Expression, Literal},
        token::{Token, TokenType},
    };

    use super::{Interpreter, InterpreterError};

    fn get_interpreter() -> Interpreter {
        Interpreter::new()
    }

    fn get_num_exp(num: f64) -> Expression {
        Expression::Literal(Literal::Number(num))
    }

    fn get_token(token_type: TokenType) -> Token {
        Token {
            token_type,
            index: 0,
        }
    }

    #[test]
    fn binary() -> Result<(), InterpreterError> {
        let operations = vec![
            (4.0, TokenType::Minus, 2.0, Literal::Number(2.0)),
            (2.0, TokenType::Plus, 2.0, Literal::Number(4.0)),
            (6.0, TokenType::Slash, 3.0, Literal::Number(2.0)),
            (2.0, TokenType::Star, 3.0, Literal::Number(6.0)),
            (4.0, TokenType::Greater, 2.0, Literal::Bool(true)),
            (2.0, TokenType::GreaterEqual, 2.0, Literal::Bool(true)),
            (4.0, TokenType::Less, 2.0, Literal::Bool(false)),
            (2.0, TokenType::LessEqual, 2.0, Literal::Bool(true)),
        ];

        for (left, token, right, expected) in operations {
            let result = get_interpreter().binary(
                &get_num_exp(left),
                &get_token(token),
                &get_num_exp(right),
            )?;

            let result = match result {
                Types::Primitive(literal) => literal,
                Types::Reference(reference_types) => {
                    panic!("Unexpected reference '{}'", reference_types)
                }
            };

            assert!(
                result == expected,
                "Did not return expected result; expected '{}' got '{}'",
                result,
                expected
            );
        }

        Ok(())
    }
}
