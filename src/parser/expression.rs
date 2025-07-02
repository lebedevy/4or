use std::fmt::Debug;

use crate::token::Token;

#[derive(Debug)]
pub(super) struct Binary {
    pub(super) left: Box<dyn Expr>,
    pub(super) operator: Token,
    pub(super) right: Box<dyn Expr>,
}

#[derive(Debug)]
pub(super) struct Grouping {
    pub(super) expression: Box<dyn Expr>,
}

#[derive(Debug)]
pub(super) enum Literal {
    Bool(bool),
    Number(f64),
    String(String),
    Nil,
}

#[derive(Debug)]
pub(super) struct Unary {
    pub(super) operator: Token,
    pub(super) right: Box<dyn Expr>,
}

pub(crate) trait Expr: Debug {}

impl Expr for Binary {}
impl Expr for Unary {}
impl Expr for Literal {}
impl Expr for Grouping {}
