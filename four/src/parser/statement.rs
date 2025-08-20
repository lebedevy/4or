use std::sync::atomic::{AtomicUsize, Ordering};

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
    pub(crate) global_id: usize,
}

impl Identifier {
    pub(crate) fn new(ident: &str, token: Token) -> Self {
        Self {
            global_id: get_id(),
            ident: ident.to_string(),
            token,
        }
    }
}

static COUNTER: AtomicUsize = AtomicUsize::new(1);

fn get_id() -> usize {
    COUNTER.fetch_add(1, Ordering::Relaxed)
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
