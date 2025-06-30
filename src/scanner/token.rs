use core::fmt;
use std::{
    iter::{Enumerate, Peekable},
    num::ParseFloatError,
    str::Chars,
};

use token_type::TokenType;

mod token_type;

#[derive(Debug)]
pub(super) enum TokenError {
    InvalidToken(char, usize),
    EmptyIterator,
    UnterminatedString(usize),
    InvalidNumber(ParseFloatError),
}

#[derive(Debug)]
pub(crate) struct Token {
    token_type: TokenType,
}

type I<'a> = Peekable<Enumerate<Chars<'a>>>;

impl From<ParseFloatError> for TokenError {
    fn from(value: ParseFloatError) -> Self {
        TokenError::InvalidNumber(value)
    }
}

impl fmt::Display for TokenError {
    // TODO: Write out the offending character
    fn fmt<'a>(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TokenError::InvalidToken(char, ind) =>
                    format!("Token {} at postion {} is not a valid token", char, ind),
                TokenError::EmptyIterator => "Attempting to read empty iterator".into(),
                TokenError::UnterminatedString(ind) =>
                    format!("String starting at {} is not terminated", ind),
                TokenError::InvalidNumber(parse_float_error) =>
                    format!("Invalid number: {}", parse_float_error),
            }
        )
    }
}

impl Token {
    pub(super) fn read_next(iter: &mut I) -> Result<Option<Self>, TokenError> {
        let token = Token::from(iter)?;

        Ok(match token {
            Some(token_type) => Some(Self { token_type }),
            None => None,
        })
    }

    fn from(iter: &mut I) -> Result<Option<TokenType>, TokenError> {
        let token = match iter.next() {
            Some((ind, c)) => match c {
                '(' => Some(TokenType::LeftParen),
                ')' => Some(TokenType::RightParen),
                '{' => Some(TokenType::LeftBrace),
                '}' => Some(TokenType::RightBrace),
                ',' => Some(TokenType::Comma),
                '.' => Some(TokenType::Dot),
                '-' => Some(TokenType::Minus),
                '+' => Some(TokenType::Plus),
                ';' => Some(TokenType::Semicolon),
                '*' => Some(TokenType::Star),
                // Potentially compound tokens
                '!' if Token::consume_if(iter, &'=') => Some(TokenType::BangEqual),
                '!' => Some(TokenType::Bang),
                '=' if Token::consume_if(iter, &'=') => Some(TokenType::EqualEqual),
                '=' => Some(TokenType::Equal),
                '<' if Token::consume_if(iter, &'=') => Some(TokenType::LessEqual),
                '<' => Some(TokenType::Less),
                '>' if Token::consume_if(iter, &'=') => Some(TokenType::GreaterEqual),
                '>' => Some(TokenType::Greater),
                // division or comment
                '/' if Token::consume_if(iter, &'/') => {
                    Token::consume_comment(iter);
                    None
                }
                '/' => Some(TokenType::Slash),
                // Strings
                '"' => Some(TokenType::String(Token::consume_string(iter, ind)?)),
                // ignored chars
                // TODO: Increment on line on new line
                ' ' | '\r' | '\t' | '\n' => None,
                // Numbers
                c if c.is_digit(10) => Some(TokenType::Number(Token::consume_number(iter, c)?)),
                _ => {
                    return Err(TokenError::InvalidToken(c, ind));
                }
            },
            None => {
                return Err(TokenError::EmptyIterator);
            }
        };

        Ok(token)
    }

    fn consume_if(iter: &mut I, next: &char) -> bool {
        iter.next_if(|(_, ch)| ch == next).is_some()
    }

    fn consume_comment(iter: &mut I) {
        while iter.next_if(|(_ind, ch)| ch != &'\n').is_some() {}
    }

    fn consume_number(iter: &mut I, start: char) -> Result<f64, TokenError> {
        let mut text = String::from(start);

        while let Some((_ind, digit)) = iter.next_if(|(_ind, ch)| ch.is_digit(10)) {
            text.push(digit);
        }

        Ok(text.parse()?)
    }

    fn consume_string(iter: &mut I, start: usize) -> Result<String, TokenError> {
        let mut text = String::new();
        while let Some((_ind, ch)) = iter.next() {
            match ch {
                '"' => return Ok(text),
                ch => text.push(ch),
            }
        }

        // if we are here, we have read the whole iterator and never reached the closing tag for
        // the string
        Err(TokenError::UnterminatedString(start))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn get_iter(content: &'static str) -> I<'static> {
        content.chars().enumerate().peekable()
    }

    #[test]
    fn consume_string_returns_content() -> Result<(), TokenError> {
        let value = "test\"";
        let mut iter = get_iter(value);

        let token = Token::consume_string(&mut iter, 0)?;
        assert_eq!(token, "test");
        Ok(())
    }

    #[test]
    fn consume_string_unterminated_string_returns_error() {
        let value = "test";
        let start = 0;
        let mut iter = get_iter(value);

        let token = Token::consume_string(&mut iter, start);
        assert!(token.is_err_and(|e| {
            match e {
                TokenError::UnterminatedString(ind) => ind == start,
                _ => false,
            }
        }));
    }

    #[test]
    fn consume_if_on_match_returns_true() {
        assert!(Token::consume_if(&mut get_iter("="), &'='));
    }

    #[test]
    fn consume_if_on_match_consumes_next_token() {
        let mut iter = get_iter("=");
        assert!(Token::consume_if(&mut iter, &'='));
        assert!(iter.next().is_none());
    }

    #[test]
    fn consume_if_no_match_returns_false() {
        assert!(!Token::consume_if(&mut get_iter("-"), &'='));
    }

    #[test]
    fn consume_if_on_empty_returns_false() {
        assert!(!Token::consume_if(&mut get_iter(""), &'='));
    }

    #[test]
    fn consume_comment_reads_whole_line() {
        let mut iter = get_iter("/ some comment");
        Token::consume_comment(&mut iter);
        assert!(iter.peek().is_none())
    }

    #[test]
    fn consume_comment_only_reads_current_line() {
        let mut iter = get_iter("/ some comment\nlet g = 123;");
        Token::consume_comment(&mut iter);
        assert!(!iter.peek().is_none())
    }

    #[test]
    fn consume_number() -> Result<(), TokenError> {
        // the first digit is consumed in the iterator
        let mut iter = get_iter("23123");
        let num = Token::consume_number(&mut iter, '1')?;
        assert_eq!(num, 123123.0);
        Ok(())
    }

    #[test]
    fn consume_decimal_number() -> Result<(), TokenError> {
        // the first digit is consumed in the iterator
        let mut iter = get_iter("23.123");
        let num = Token::consume_number(&mut iter, '1')?;
        assert_eq!(num, 123.123);
        Ok(())
    }
}
