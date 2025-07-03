use std::fmt::Debug;

use crate::token::Token;

#[derive(Debug)]
pub(crate) enum Expression {
    Binary(Box<Expression>, Token, Box<Expression>),
    Grouping(Box<Expression>),
    Literal(Literal),
    Unary(Token, Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Literal {
    Bool(bool),
    Number(f64),
    String(String),
    Nil,
}
