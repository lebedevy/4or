use crate::{
    parser::{Expression, Literal, Statement},
    token::{Token, TokenType},
};

pub(super) struct Interpreter {}

impl Interpreter {
    pub(super) fn run(statements: Vec<Statement>) -> Result<(), InterpreterError> {
        for statement in statements {
            Interpreter::execute(statement)?;
        }

        Ok(())
    }

    fn execute(statement: Statement) -> Result<(), InterpreterError> {
        match statement {
            Statement::Expression(expression) => {
                Interpreter::evaluate(expression)?;
            }
            Statement::Print(expression) => {
                let val = Interpreter::evaluate(expression)?;
                println!("> {}", val);
            }
        };

        Ok(())
    }

    fn evaluate(expression: Expression) -> Result<Literal, InterpreterError> {
        expression.evaluate()
    }
}

#[derive(Debug)]
pub(super) enum InterpreterError {
    InvalidUnary(Token),
    InvalidBinary(Token),
}

trait Evaluate {
    fn evaluate(&self) -> Result<Literal, InterpreterError>;
}

impl Evaluate for Expression {
    fn evaluate(&self) -> Result<Literal, InterpreterError> {
        match self {
            Expression::Binary(left, token, right) => binary(left, token, right),
            Expression::Grouping(expression) => grouping(expression),
            // TODO: Do we want to clone?
            Expression::Literal(literal) => Ok(literal.clone()),
            Expression::Unary(token, expression) => unary(token, expression),
        }
    }
}

fn binary(
    left: &Box<Expression>,
    operator: &Token,
    right: &Box<Expression>,
) -> Result<Literal, InterpreterError> {
    let left = left.evaluate()?;
    let right = right.evaluate()?;

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

fn unary(token: &Token, expression: &Box<Expression>) -> Result<Literal, InterpreterError> {
    let right = expression.evaluate()?;

    let res = match (&token.token_type, right) {
        (TokenType::Minus, Literal::Number(num)) => Literal::Number(-num),
        // We are not allowing the concept of "truthiness"; give me bool or get bonked
        (TokenType::Bang, Literal::Bool(val)) => Literal::Bool(!val),
        _val => return Err(InterpreterError::InvalidUnary(token.clone())),
    };

    Ok(res)
}

fn grouping(expression: &Box<Expression>) -> Result<Literal, InterpreterError> {
    expression.evaluate()
}
