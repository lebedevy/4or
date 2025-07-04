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
            self.execute(statement)?;
        }

        Ok(())
    }

    fn execute(&mut self, statement: Statement) -> Result<(), InterpreterError> {
        match statement {
            Statement::Expression(expression) => {
                expression.evaluate(&self.environment)?;
            }
            Statement::Print(expression) => {
                let val = expression.evaluate(&self.environment)?;
                println!("> {}", val);
            }
            Statement::Variable(token, expression) => {
                let value = match expression {
                    Some(expression) => Some(expression.evaluate(&self.environment)?),
                    None => None,
                };

                let identifier = match token.token_type {
                    TokenType::Identifier(iden) => iden,
                    _ => return Err(InterpreterError::InvalidVariable(token)),
                };

                self.environment.define(
                    identifier,
                    match value {
                        Some(val) => val,
                        None => Literal::Nil,
                    },
                );
            }
        };

        Ok(())
    }
}

#[derive(Debug)]
pub(super) enum InterpreterError {
    InvalidUnary(Token),
    InvalidBinary(Token),
    InvalidVariable(Token),
    UndefinedVariable(String),
}

trait Evaluate {
    fn evaluate(&self, environment: &Environment) -> Result<Literal, InterpreterError>;
}

impl Evaluate for Expression {
    fn evaluate(&self, environment: &Environment) -> Result<Literal, InterpreterError> {
        match self {
            Expression::Binary(left, token, right) => binary(left, token, right, environment),
            Expression::Grouping(expression) => grouping(expression, environment),
            Expression::Literal(literal) => Ok(literal.clone()),
            Expression::Unary(token, expression) => unary(token, expression, environment),
            Expression::Variable(token) => variable(token, environment),
        }
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

fn variable(token: &Token, environment: &Environment) -> Result<Literal, InterpreterError> {
    match &token.token_type {
        TokenType::Identifier(iden) => Ok(environment.get(iden)?),
        _ => return Err(InterpreterError::InvalidVariable(token.clone())),
    }
}

fn binary(
    left: &Box<Expression>,
    operator: &Token,
    right: &Box<Expression>,
    environment: &Environment,
) -> Result<Literal, InterpreterError> {
    let left = left.evaluate(environment)?;
    let right = right.evaluate(environment)?;

    // TODO: Overflow
    let res = match (&operator.token_type, left, right) {
        // Numeric
        (TokenType::Minus, Literal::Number(right), Literal::Number(left)) => {
            Literal::Number(left - right)
        }
        (TokenType::Plus, Literal::Number(right), Literal::Number(left)) => {
            Literal::Number(left + right)
        }
        (TokenType::Slash, Literal::Number(right), Literal::Number(left)) => {
            Literal::Number(left / right)
        }
        (TokenType::Star, Literal::Number(right), Literal::Number(left)) => {
            Literal::Number(left * right)
        }
        // Strings
        (TokenType::Plus, Literal::String(right), Literal::String(left)) => {
            Literal::String(left + &right)
        }
        // Comparison
        (TokenType::Greater, Literal::Number(right), Literal::Number(left)) => {
            Literal::Bool(left > right)
        }
        (TokenType::GreaterEqual, Literal::Number(right), Literal::Number(left)) => {
            Literal::Bool(left >= right)
        }
        (TokenType::Less, Literal::Number(right), Literal::Number(left)) => {
            Literal::Bool(left < right)
        }
        (TokenType::LessEqual, Literal::Number(right), Literal::Number(left)) => {
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
    token: &Token,
    expression: &Box<Expression>,
    environment: &Environment,
) -> Result<Literal, InterpreterError> {
    let right = expression.evaluate(environment)?;

    let res = match (&token.token_type, right) {
        (TokenType::Minus, Literal::Number(num)) => Literal::Number(-num),
        // We are not allowing the concept of "truthiness"; give me bool or get bonked
        (TokenType::Bang, Literal::Bool(val)) => Literal::Bool(!val),
        _val => return Err(InterpreterError::InvalidUnary(token.clone())),
    };

    Ok(res)
}

fn grouping(
    expression: &Box<Expression>,
    environment: &Environment,
) -> Result<Literal, InterpreterError> {
    expression.evaluate(environment)
}
