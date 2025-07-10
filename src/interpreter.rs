use std::fmt::Display;

use crate::{
    environment::{Environment, EnvironmentError},
    parser::{Expression, Literal, Statement},
    token::{Token, TokenType},
};

pub(super) struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    pub(super) fn new() -> Self {
        Self {
            environment: Environment::new(),
        }
    }

    pub(super) fn run(&mut self, statements: Vec<Statement>) -> Result<(), InterpreterError> {
        for statement in statements {
            self.execute(&statement)?;
        }

        Ok(())
    }

    fn execute(&mut self, statement: &Statement) -> Result<(), InterpreterError> {
        match statement {
            Statement::Expression(expression) => {
                self.evaluate(&expression)?;
            }
            Statement::Print(expression) => {
                let val = self.evaluate(&expression)?;
                println!("> {}", val);
            }
            Statement::Variable(token, expression) => {
                let value = match expression {
                    Some(expression) => Some(self.evaluate(&expression)?),
                    None => None,
                };

                let identifier = match &token.token_type {
                    TokenType::Identifier(iden) => iden,
                    _ => return Err(InterpreterError::InvalidVariableIdentifier(token.clone())),
                };

                self.environment.define(
                    identifier,
                    match value {
                        Some(val) => val,
                        None => Literal::Nil,
                    },
                )?;
            }
            Statement::Block(statements) => {
                self.environment.create_scope();

                for statement in statements {
                    self.execute(statement)?;
                }

                self.environment.pop_scope();
            }
            Statement::If(expression, statement, else_statement) => {
                match self.evaluate(&expression)? {
                    Literal::Bool(val) => {
                        if val {
                            self.execute(statement)?;
                        } else {
                            if let Some(statement) = else_statement {
                                self.execute(statement)?;
                            }
                        }
                    }
                    token => return Err(InterpreterError::ExpectedBool(token)),
                };
            }
            Statement::While(expression, statement) => loop {
                match self.evaluate(&expression)? {
                    Literal::Bool(condition) => {
                        if !condition {
                            break;
                        }
                    }
                    condition => return Err(InterpreterError::ExpectedBool(condition)),
                }

                self.execute(statement)?;
            },
        };

        Ok(())
    }
}

#[derive(Debug)]
pub(super) enum InterpreterError {
    InvalidUnary(Token),
    InvalidBinary(Token),
    InvalidLogicalOperator(Token),
    InvalidVariableIdentifier(Token),
    UndefinedVariable(String),
    UndefinedScope,
    ExpectedBool(Literal),
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
            InterpreterError::UndefinedScope => write!(f, "Undefined scope")?,
            InterpreterError::ExpectedBool(literal) => {
                write!(f, "Interpreter error: Expected bool, got '{}'", literal)?
            }
            InterpreterError::InvalidLogicalOperator(token) => write!(
                f,
                "Interpreter error: Expected 'and' or 'or', got '{}'",
                token
            )?,
        };

        Ok(())
    }
}

impl Interpreter {
    fn evaluate(&mut self, expression: &Expression) -> Result<Literal, InterpreterError> {
        match expression {
            Expression::Binary(left, token, right) => self.binary(left, token, right),
            Expression::Grouping(expression) => self.grouping(expression),
            // TODO: consider implications of cloning
            Expression::Literal(literal) => Ok(literal.clone()),
            Expression::Unary(token, expression) => self.unary(token, expression),
            Expression::Variable(token) => self.variable(token),
            Expression::Assignment(token, expression) => self.assign(token, expression),
            Expression::Logical(left, operator, right) => self.logical(left, operator, right),
        }
    }

    fn logical(
        &mut self,
        left: &Expression,
        operator: &Token,
        right: &Expression,
    ) -> Result<Literal, InterpreterError> {
        let left = match self.evaluate(left)? {
            Literal::Bool(val) => val,
            left => return Err(InterpreterError::ExpectedBool(left)),
        };

        // if and and false, return false; if or and true, return true
        match &operator.token_type {
            TokenType::And => {
                if !left {
                    return Ok(Literal::Bool(left));
                }
            }
            TokenType::Or => {
                if left {
                    return Ok(Literal::Bool(left));
                }
            }
            _ => return Err(InterpreterError::InvalidLogicalOperator(operator.clone())),
        };

        Ok(self.evaluate(right)?)
    }

    fn variable(&self, token: &Token) -> Result<Literal, InterpreterError> {
        match &token.token_type {
            TokenType::Identifier(iden) => Ok(self.environment.get(iden)?),
            _ => return Err(InterpreterError::InvalidVariableIdentifier(token.clone())),
        }
    }

    fn assign(
        &mut self,
        token: &Token,
        expression: &Expression,
    ) -> Result<Literal, InterpreterError> {
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
    ) -> Result<Literal, InterpreterError> {
        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;

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

        Ok(res)
    }

    fn unary(
        &mut self,
        token: &Token,
        expression: &Expression,
    ) -> Result<Literal, InterpreterError> {
        let right = self.evaluate(expression)?;

        let res = match (&token.token_type, right) {
            (TokenType::Minus, Literal::Number(num)) => Literal::Number(-num),
            // We are not allowing the concept of "truthiness"; give me bool or get bonked
            (TokenType::Bang, Literal::Bool(val)) => Literal::Bool(!val),
            _val => return Err(InterpreterError::InvalidUnary(token.clone())),
        };

        Ok(res)
    }

    fn grouping(&mut self, expression: &Expression) -> Result<Literal, InterpreterError> {
        self.evaluate(expression)
    }
}

impl From<EnvironmentError> for InterpreterError {
    fn from(value: EnvironmentError) -> Self {
        match value {
            EnvironmentError::UndefinedVariable(value) => {
                InterpreterError::UndefinedVariable(value)
            }
            EnvironmentError::UndefinedScope => InterpreterError::UndefinedScope,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
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
