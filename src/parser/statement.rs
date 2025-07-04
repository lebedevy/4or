use crate::token::Token;

use super::Expression;

#[derive(Debug)]
pub(crate) enum Statement {
    Expression(Expression),
    Print(Expression),
    Variable(Token, Option<Expression>),
}
