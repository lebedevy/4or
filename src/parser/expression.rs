use std::fmt::{Debug, Display};

use crate::token::Token;

#[derive(Debug, PartialEq)]
pub(crate) enum Expression {
    Binary(Box<Expression>, Token, Box<Expression>),
    Grouping(Box<Expression>),
    Literal(Literal),
    Variable(Token),
    Unary(Token, Box<Expression>),
    Assignment(Token, Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Literal {
    Bool(bool),
    Number(f64),
    String(String),
    Nil,
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Bool(val) => {
                write!(f, "{}", val)?;
            }
            Literal::Number(val) => {
                write!(f, "{}", val)?;
            }
            Literal::String(val) => {
                write!(f, "{}", val)?;
            }
            Literal::Nil => {
                write!(f, "nil")?;
            }
        };
        Ok(())
    }
}
