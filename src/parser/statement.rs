use super::Expression;

pub(crate) enum Statement {
    Expression(Expression),
    Print(Expression),
}
