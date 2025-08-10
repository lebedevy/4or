use crate::token::Token;

use super::Expression::{self};

#[derive(Debug, Clone)]
pub(crate) struct Block {
    pub(crate) statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Identifier {
    pub(crate) ident: String,
    pub(crate) token: Token,
}

#[derive(Debug, Clone)]
pub(crate) enum Statement {
    Expression(Expression),
    Variable(Identifier, Option<Expression>),
    Block(Block),
    Function(Identifier, Vec<Identifier>, Block),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    While(Expression, Box<Statement>),
    Return(Token, Box<Statement>),
}
