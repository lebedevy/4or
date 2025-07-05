use std::{
    fmt::Display,
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
    UnexpectedToken(Option<Token>, TokenType),
    InvalidPrimaryToken(Token),
    UnexpectedTermination,
    ExpectedIdentifier(Option<Token>),
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::UnexpectedToken(actual, expected) => {
                write!(
                    f,
                    "Parser error: unexpected token; expected '{}', got '{}'",
                    expected,
                    actual
                        .as_ref()
                        .map_or("None".to_string(), |x| x.to_string())
                )?;
            }
            ParserError::InvalidPrimaryToken(token) => {
                write!(f, "Parser error: Invalid primary token - '{}'", token)?;
            }
            ParserError::UnexpectedTermination => {
                write!(f, "Parser error: Unexpected end in token terminator")?;
            }
            ParserError::ExpectedIdentifier(token) => {
                write!(
                    f,
                    "Parser error: Expected identifier, got '{}'",
                    token.as_ref().map_or("None".to_string(), |x| x.to_string())
                )?;
            }
        };

        Ok(())
    }
}

impl Parser {
    pub(crate) fn parse(mut tokens: Peek) -> Result<Vec<Statement>, ParserError> {
        let mut statements = vec![];
        while let Some((_, token)) = tokens.peek() {
            let res = match token.token_type {
                TokenType::Print => {
                    tokens.next();
                    Parser::print_statement(&mut tokens)
                }
                TokenType::Let => Parser::variable(&mut tokens),
                _ => Parser::expression_statement(&mut tokens),
            };

            match res {
                Ok(res) => statements.push(res),
                Err(err) => {
                    Parser::synchronize(&mut tokens);
                    return Err(err);
                }
            }
        }

        Ok(statements)
    }

    fn variable(tokens: &mut Peek) -> Result<Statement, ParserError> {
        Parser::expect_match(tokens, TokenType::Let)?;
        let Some((_, identifier)) = tokens.next() else {
            return Err(ParserError::ExpectedIdentifier(None));
        };

        let identifier = match &identifier.token_type {
            TokenType::Identifier(_) => identifier,
            _token => {
                return Err(ParserError::UnexpectedToken(
                    Some(identifier.clone()),
                    TokenType::Identifier("".to_string()),
                ));
            }
        };

        let initial = match Parser::match_next(tokens, &vec![TokenType::Equal]) {
            // we are doing assingment
            Some((_, token)) if token.token_type == TokenType::Equal => {
                Some(Parser::expression(tokens)?)
            }
            _ => None,
        };

        Parser::expect_match(tokens, TokenType::Semicolon)?;

        Ok(Statement::Variable(identifier, initial))
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
        Parser::assignment(tokens)
    }

    fn assignment(tokens: &mut Peek) -> Result<Expression, ParserError> {
        let expr = Parser::equality(tokens)?;

        if let Some((_, equals)) = Parser::match_next(tokens, &vec![TokenType::Equal]) {
            let value = Parser::expression(tokens)?;

            // we can assign if the target is a variable expression; otherwise report error
            match expr {
                Expression::Variable(variable) => {
                    return Ok(Expression::Assignment(variable, Box::new(value)));
                }
                // report error; do not throw it
                _ => eprintln!("Invalid assignment target at {}", equals),
            };
        }

        Ok(expr)
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

    // TODO: Consider empty expressions
    fn primary(tokens: &mut Peek) -> Result<Expression, ParserError> {
        let res = match tokens.next() {
            Some((_, token)) => match token.token_type {
                TokenType::False => Expression::Literal(Literal::Bool(false)),
                TokenType::True => Expression::Literal(Literal::Bool(true)),
                TokenType::Nil => Expression::Literal(Literal::Nil),
                TokenType::Number(num) => Expression::Literal(Literal::Number(num)),
                TokenType::String(str) => Expression::Literal(Literal::String(str)),
                TokenType::Identifier(_) => Expression::Variable(token),
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
        while let Some((_, token)) = tokens.peek() {
            match token.token_type {
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
            }
        }
    }

    fn expect_match(tokens: &mut Peek, expected: TokenType) -> Result<Token, ParserError> {
        match tokens.next() {
            Some(actual) => match actual {
                (_, actual) if actual.token_type == expected => Ok(actual),
                (_, actual) => Err(ParserError::UnexpectedToken(Some(actual), expected)),
            },
            None => Err(ParserError::UnexpectedToken(None, expected)),
        }
    }

    fn match_next(tokens: &mut Peek, to_match: &Vec<TokenType>) -> Option<(usize, Token)> {
        tokens.next_if(|(_, ch)| to_match.contains(&ch.token_type))
    }
}

#[cfg(test)]
mod tests {
    use core::panic;

    use crate::{
        parser::{Expression, Literal, Statement},
        token::{Token, TokenType},
    };

    use super::{Parser, ParserError, Peek};

    fn get_single(token_type: TokenType) -> Peek {
        vec![Token {
            token_type,
            index: 0,
        }]
        .into_iter()
        .enumerate()
        .peekable()
    }

    fn get_iter(tokens: Vec<TokenType>) -> Peek {
        tokens
            .into_iter()
            .enumerate()
            .map(|(index, token_type)| Token { token_type, index })
            .collect::<Vec<Token>>()
            .into_iter()
            .enumerate()
            .peekable()
    }

    // Single rules
    #[test]
    fn primary_false() -> Result<(), ParserError> {
        let literal = Parser::primary(&mut get_single(TokenType::False))?;
        assert_eq!(literal, Expression::Literal(Literal::Bool(false)));
        Ok(())
    }

    #[test]
    fn primary_true() -> Result<(), ParserError> {
        let literal = Parser::primary(&mut get_single(TokenType::True))?;
        assert_eq!(literal, Expression::Literal(Literal::Bool(true)));
        Ok(())
    }

    #[test]
    fn primary_nil() -> Result<(), ParserError> {
        let literal = Parser::primary(&mut get_single(TokenType::Nil))?;
        assert_eq!(literal, Expression::Literal(Literal::Nil));
        Ok(())
    }

    #[test]
    fn primary_number() -> Result<(), ParserError> {
        let literal = Parser::primary(&mut get_single(TokenType::Number(123.123)))?;
        assert_eq!(literal, Expression::Literal(Literal::Number(123.123)));
        Ok(())
    }

    #[test]
    fn primary_string() -> Result<(), ParserError> {
        let literal = Parser::primary(&mut get_single(TokenType::String("test".to_string())))?;
        assert_eq!(
            literal,
            Expression::Literal(Literal::String("test".to_string()))
        );
        Ok(())
    }

    #[test]
    fn primary_identifier() -> Result<(), ParserError> {
        let literal = Parser::primary(&mut get_single(TokenType::Identifier("test".to_string())))?;
        assert_eq!(
            literal,
            Expression::Variable(Token {
                token_type: TokenType::Identifier("test".to_string()),
                index: 0
            })
        );
        Ok(())
    }

    // Compound rules
    // parenthesis
    #[test]
    fn closed_paren() -> Result<(), ParserError> {
        let literal = Parser::primary(&mut get_iter(vec![
            TokenType::LeftParen,
            TokenType::True,
            TokenType::RightParen,
        ]))?;

        assert!(matches!(literal, Expression::Grouping(_)));
        Ok(())
    }

    #[test]
    fn unclosed_paren_error() {
        let err = Parser::primary(&mut get_iter(vec![TokenType::LeftParen, TokenType::True]));
        assert!(
            err.is_err_and(|e| matches!(e, ParserError::UnexpectedToken(_, TokenType::RightParen)))
        )
    }

    // declaration
    #[test]
    fn variable_assigned() -> Result<(), ParserError> {
        let parsed = Parser::parse(get_iter(vec![
            TokenType::Let,
            TokenType::Identifier("test".to_string()),
            TokenType::Equal,
            TokenType::True,
            TokenType::Semicolon,
        ]))?;

        assert!(parsed.len() == 1, "Unexpected # of statements");

        let (token, expression) = match &parsed[0] {
            Statement::Variable(token, expression) => (token, expression),
            _ => panic!("Did not produce a variable"),
        };

        assert!(matches!(
                &token.token_type,
                TokenType::Identifier(val) if val == "test"
        ));

        let Some(expression) = expression else {
            panic!("Missing expected expression");
        };

        assert!(
            matches!(expression, Expression::Literal(literal) if literal == &Literal::Bool(true))
        );

        Ok(())
    }

    #[test]
    fn variable_unassigned() -> Result<(), ParserError> {
        let parsed = Parser::parse(get_iter(vec![
            TokenType::Let,
            TokenType::Identifier("test".to_string()),
            TokenType::Semicolon,
        ]))?;

        assert!(parsed.len() == 1, "Unexpected # of statements");

        let (token, expression) = match &parsed[0] {
            Statement::Variable(token, expression) => (token, expression),
            _ => panic!("Did not produce a variable"),
        };

        assert!(matches!(
                &token.token_type,
                TokenType::Identifier(val) if val == "test"
        ));

        assert!(expression.is_none());

        Ok(())
    }

    #[test]
    fn variable_no_assignment_panics() -> Result<(), ParserError> {
        let err = Parser::parse(get_iter(vec![
            TokenType::Let,
            TokenType::Identifier("test".to_string()),
            TokenType::Equal,
            TokenType::Semicolon,
        ]));

        assert!(err.is_err_and(|e| matches!(e, ParserError::InvalidPrimaryToken(_,))));

        Ok(())
    }

    #[test]
    fn variable_no_identifier_panics() -> Result<(), ParserError> {
        let err = Parser::parse(get_iter(vec![TokenType::Let, TokenType::Equal]));

        assert!(err.is_err_and(|e| matches!(
            e,
            ParserError::UnexpectedToken(_, TokenType::Identifier(_))
        )));

        Ok(())
    }

    // assignment
    #[test]
    fn assignment() -> Result<(), ParserError> {
        // semicolon is not needed because this is an expression, not a statement
        Parser::assignment(&mut get_iter(vec![
            TokenType::Identifier("test".to_string()),
            TokenType::Equal,
            TokenType::True,
        ]))?;

        Ok(())
    }

    #[test]
    fn assignment_expression() -> Result<(), ParserError> {
        Parser::assignment(&mut get_iter(vec![
            TokenType::Identifier("test".to_string()),
            TokenType::Equal,
            TokenType::Number(4.0),
            TokenType::Plus,
            TokenType::Number(6.0),
        ]))?;

        Ok(())
    }

    // TODO: Need to test for failure condition, but no simple way to check console output w/out
    // adding a crate
    #[ignore]
    #[test]
    fn assignment_to_invalid_expression_prints_error() -> Result<(), ParserError> {
        Parser::assignment(&mut get_iter(vec![
            TokenType::Number(4.0),
            TokenType::Plus,
            TokenType::Number(6.0),
            TokenType::Equal,
            TokenType::Identifier("test".to_string()),
        ]))?;

        Ok(())
    }
}
