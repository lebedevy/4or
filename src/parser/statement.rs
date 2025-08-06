use crate::token::Token;

use super::Expression::{self};
use crate::token::TokenType::Identifier;

#[derive(Debug, Clone)]
pub(crate) struct Block {
    pub(crate) statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub(crate) enum Statement {
    Expression(Expression),
    Variable(Identifier, Option<Expression>),
    Block(Block),
    Function(Token, Vec<Token>, Block),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    While(Expression, Box<Statement>),
    Return(Token, Box<Statement>),
}
