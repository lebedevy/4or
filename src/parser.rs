use std::{
    iter::{Enumerate, Peekable},
    vec::IntoIter,
};

use expression::{Binary, Expr, Grouping, Literal, Unary};

use crate::token::{Token, TokenType};

mod expression;

type Peek = Peekable<Enumerate<IntoIter<Token>>>;

pub(super) struct Parser {}

impl Parser {
    pub(crate) fn parse(mut tokens: Peek) -> Box<dyn Expr> {
        Parser::expression(&mut tokens)
    }

    fn expression(tokens: &mut Peek) -> Box<dyn Expr> {
        Parser::equality(tokens)
    }

    fn equality(tokens: &mut Peek) -> Box<dyn Expr> {
        Parser::consume(
            tokens,
            vec![TokenType::BangEqual, TokenType::EqualEqual],
            Parser::comparison,
        )
    }

    // TODO: This is the same function as above, except for the tokens we match
    fn comparison(tokens: &mut Peek) -> Box<dyn Expr> {
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

    fn term(tokens: &mut Peek) -> Box<dyn Expr> {
        Parser::consume(
            tokens,
            vec![TokenType::Minus, TokenType::Plus],
            Parser::factor,
        )
    }

    fn factor(tokens: &mut Peek) -> Box<dyn Expr> {
        Parser::consume(
            tokens,
            vec![TokenType::Slash, TokenType::Star],
            Parser::unary,
        )
    }

    fn unary(tokens: &mut Peek) -> Box<dyn Expr> {
        while let Some((_, operator)) =
            Parser::match_next(tokens, &vec![TokenType::Bang, TokenType::Minus])
        {
            let right = Parser::term(tokens);
            return Box::new(Unary { operator, right });
        }

        return Parser::primary(tokens);
    }

    // TODO: More graceful handing of exists
    fn primary(tokens: &mut Peek) -> Box<dyn Expr> {
        match tokens.next() {
            Some((_, token)) => match token.token_type {
                TokenType::False => Box::new(Literal::Bool(false)),
                TokenType::True => Box::new(Literal::Bool(true)),
                TokenType::Nil => Box::new(Literal::Nil),
                TokenType::Number(num) => Box::new(Literal::Number(num)),
                TokenType::String(str) => Box::new(Literal::String(str)),
                TokenType::LeftParen => {
                    let expr = Parser::expression(tokens);
                    assert!(
                        Parser::match_next(tokens, &vec![TokenType::RightParen]).is_some(),
                        "Parenthesis not closed"
                    );
                    Box::new(Grouping { expression: expr })
                }
                _ => panic!("Invalid token during parsing"),
            },
            None => panic!("Unexpected termination during parsking"),
        }
    }

    fn consume(
        tokens: &mut Peek,
        operators: Vec<TokenType>,
        mut expr: impl FnMut(&mut Peek) -> Box<dyn Expr>,
    ) -> Box<dyn Expr> {
        let mut left = expr(tokens);

        while let Some((_, operator)) = Parser::match_next(tokens, &operators) {
            let right = expr(tokens);
            left = Box::new(Binary {
                left,
                operator,
                right,
            })
        }

        left
    }

    fn syncrhonize(tokens: &mut Peek) {
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

    fn match_next(tokens: &mut Peek, to_match: &Vec<TokenType>) -> Option<(usize, Token)> {
        tokens.next_if(|(_, ch)| to_match.contains(&ch.token_type))
    }
}
