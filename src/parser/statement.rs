use crate::token::Token;

use super::Expression;

#[derive(Debug, Clone)]
pub(crate) enum Statement {
    Expression(Expression),
    Variable(Token, Option<Expression>),
    Block(Vec<Statement>),
    Function(Token, Vec<Token>, Box<Statement>),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    While(Expression, Box<Statement>),
    Return(Token, Box<Statement>),
}
