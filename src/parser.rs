use std::{
    iter::{Enumerate, Peekable},
    vec::IntoIter,
};

pub(crate) use expression::{Expression, Literal};
pub(crate) use statement::Statement;

use crate::token::{Token, TokenType};

mod expression;
mod statement;

type Peek = Peekable<Enumerate<IntoIter<Token>>>;

pub(super) struct Parser {}

#[derive(Debug)]
pub(super) enum ParserError {
    // TODO: this should be a struct holding actual/expected Tokens
    UnterminatedToken(Token, TokenType),
    InvalidPrimaryToken(Token),
    UnexpectedTermination,
}

impl Parser {
    pub(crate) fn parse(mut tokens: Peek) -> Result<Vec<Statement>, ParserError> {
        let mut statements = vec![];
        while let Some((_, token)) = tokens.peek() {
            let res = match token.token_type {
                TokenType::Print => {
                    tokens.next();
                    Parser::print_statement(&mut tokens)?
                }
                _ => Parser::expression_statement(&mut tokens)?,
            };

            statements.push(res);
        }

        Ok(statements)
    }

    fn print_statement(tokens: &mut Peek) -> Result<Statement, ParserError> {
        let value = Parser::expression(tokens)?;
        Parser::expect_match(tokens, TokenType::Semicolon)?;
        Ok(Statement::Print(value))
    }

    fn expression_statement(tokens: &mut Peek) -> Result<Statement, ParserError> {
        let expression = Parser::expression(tokens)?;
        Parser::expect_match(tokens, TokenType::Semicolon)?;
        Ok(Statement::Expression(expression))
    }

    fn expression(tokens: &mut Peek) -> Result<Expression, ParserError> {
        Parser::equality(tokens)
    }

    fn equality(tokens: &mut Peek) -> Result<Expression, ParserError> {
        Parser::consume(
            tokens,
            vec![TokenType::BangEqual, TokenType::EqualEqual],
            Parser::comparison,
        )
    }

    // TODO: This is the same function as above, except for the tokens we match
    fn comparison(tokens: &mut Peek) -> Result<Expression, ParserError> {
        Parser::consume(
            tokens,
            vec![
                TokenType::Greater,
                TokenType::GreaterEqual,
                TokenType::Less,
                TokenType::LessEqual,
            ],
            Parser::term,
        )
    }

    fn term(tokens: &mut Peek) -> Result<Expression, ParserError> {
        Parser::consume(
            tokens,
            vec![TokenType::Minus, TokenType::Plus],
            Parser::factor,
        )
    }

    fn factor(tokens: &mut Peek) -> Result<Expression, ParserError> {
        Parser::consume(
            tokens,
            vec![TokenType::Slash, TokenType::Star],
            Parser::unary,
        )
    }

    fn unary(tokens: &mut Peek) -> Result<Expression, ParserError> {
        while let Some((_, operator)) =
            Parser::match_next(tokens, &vec![TokenType::Bang, TokenType::Minus])
        {
            let right = Parser::term(tokens)?;
            return Ok(Expression::Unary(operator, Box::new(right)));
        }

        return Parser::primary(tokens);
    }

    fn primary(tokens: &mut Peek) -> Result<Expression, ParserError> {
        let res = match tokens.next() {
            Some((_, token)) => match token.token_type {
                TokenType::False => Expression::Literal(Literal::Bool(false)),
                TokenType::True => Expression::Literal(Literal::Bool(true)),
                TokenType::Nil => Expression::Literal(Literal::Nil),
                TokenType::Number(num) => Expression::Literal(Literal::Number(num)),
                TokenType::String(str) => Expression::Literal(Literal::String(str)),
                TokenType::LeftParen => {
                    let expr = Parser::expression(tokens)?;
                    Parser::expect_match(tokens, TokenType::RightParen)?;
                    Expression::Grouping(Box::new(expr))
                }
                _ => return Err(ParserError::InvalidPrimaryToken(token.clone())),
            },
            None => return Err(ParserError::UnexpectedTermination),
        };

        Ok(res)
    }

    fn consume(
        tokens: &mut Peek,
        operators: Vec<TokenType>,
        mut expr: impl FnMut(&mut Peek) -> Result<Expression, ParserError>,
    ) -> Result<Expression, ParserError> {
        let mut left = expr(tokens)?;

        while let Some((_, operator)) = Parser::match_next(tokens, &operators) {
            let right = expr(tokens)?;
            left = Expression::Binary(Box::new(left), operator, Box::new(right))
        }

        Ok(left)
    }

    fn synchronize(tokens: &mut Peek) {
        loop {
            match tokens.peek() {
                Some((_, token)) => match token.token_type {
                    TokenType::Semicolon => {
                        tokens.next();
                        return ();
                    }
                    TokenType::Class => return (),
                    TokenType::Fn => return (),
                    TokenType::For => return (),
                    TokenType::If => return (),
                    TokenType::Print => return (),
                    TokenType::Return => return (),
                    TokenType::Let => return (),
                    TokenType::While => return (),
                    _ => {
                        tokens.next();
                    }
                },
                None => (),
            }
        }

        // TODO: There is an advance in the book here; why?
    }

    fn expect_match(tokens: &mut Peek, token: TokenType) -> Result<(), ParserError> {
        match tokens.next() {
            Some(expected) => match expected {
                (_, expected) if expected.token_type == token => Ok(()),
                (_, actual) => Err(ParserError::UnterminatedToken(actual, token)),
            },
            None => Err(ParserError::UnexpectedTermination),
        }
    }

    fn match_next(tokens: &mut Peek, to_match: &Vec<TokenType>) -> Option<(usize, Token)> {
        tokens.next_if(|(_, ch)| to_match.contains(&ch.token_type))
    }
}
