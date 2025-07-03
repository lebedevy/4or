use core::panic;

use crate::{
    parser::{Expression, Literal},
    token::{Token, TokenType},
};

pub(super) struct Interpreter {}

impl Interpreter {
    pub(super) fn evaluate(expression: Expression) -> Literal {
        expression.evaluate()
    }
}

trait Evaluate {
    fn evaluate(&self) -> Literal;
}

impl Evaluate for Expression {
    fn evaluate(&self) -> Literal {
        match self {
            Expression::Binary(left, token, right) => binary(left, token, right),
            Expression::Grouping(expression) => grouping(expression),
            // TODO: Do we want to clone?
            Expression::Literal(literal) => literal.clone(),
            Expression::Unary(token, expression) => unary(token, expression),
        }
    }
}

fn binary(left: &Box<Expression>, operator: &Token, right: &Box<Expression>) -> Literal {
    let left = left.evaluate();
    let right = right.evaluate();

    match (&operator.token_type, left, right) {
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
        val => panic!("Unexpected binary operator {:?}", val),
    }
}

fn unary(token: &Token, expression: &Box<Expression>) -> Literal {
    let right = expression.evaluate();

    match (&token.token_type, right) {
        (TokenType::Minus, Literal::Number(num)) => Literal::Number(-num),
        // We are not allowing the concept of "truthiness"; give me bool or get bonked
        (TokenType::Bang, Literal::Bool(val)) => Literal::Bool(!val),
        val => panic!("Unexpected unary expression; {:?}", val),
    }
}

fn grouping(expression: &Box<Expression>) -> Literal {
    expression.evaluate()
}
