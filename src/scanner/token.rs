use core::fmt;
use std::{iter::Enumerate, num::ParseFloatError, str::Chars};

use itertools::MultiPeek;
use token_type::TokenType;

mod token_type;

#[derive(Debug)]
pub(super) enum TokenParseError {
    InvalidToken(char, usize),
    EmptyIterator,
    UnterminatedString(usize),
    InvalidNumber(ParseFloatError),
}

#[derive(Debug)]
pub(crate) struct Token {
    token_type: TokenType,
}

type I<'a> = MultiPeek<Enumerate<Chars<'a>>>;

impl From<ParseFloatError> for TokenParseError {
    fn from(value: ParseFloatError) -> Self {
        TokenParseError::InvalidNumber(value)
    }
}

impl fmt::Display for TokenParseError {
    // TODO: Write out the offending character
    fn fmt<'a>(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TokenParseError::InvalidToken(char, ind) =>
                    format!("Token {} at postion {} is not a valid token", char, ind),
                TokenParseError::EmptyIterator => "Attempting to read empty iterator".into(),
                TokenParseError::UnterminatedString(ind) =>
                    format!("String starting at {} is not terminated", ind),
                TokenParseError::InvalidNumber(parse_float_error) =>
                    format!("Invalid number: {}", parse_float_error),
            }
        )
    }
}

impl Token {
    pub(super) fn read_next(iter: &mut I) -> Result<Option<Self>, TokenParseError> {
        let token = Token::from(iter)?;

        Ok(match token {
            Some(token_type) => Some(Self { token_type }),
            None => None,
        })
    }

    fn from(iter: &mut I) -> Result<Option<TokenType>, TokenParseError> {
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
                '|' if Token::consume_if(iter, &'|') => Some(TokenType::Or),
                '&' if Token::consume_if(iter, &'&') => Some(TokenType::And),
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
                c if c.is_alphabetic() => Some(Token::consume_identifier(iter, c)),
                _ => {
                    return Err(TokenParseError::InvalidToken(c, ind));
                }
            },
            None => {
                return Err(TokenParseError::EmptyIterator);
            }
        };

        Ok(token)
    }

    fn next_if_matches(iter: &mut I, next: &char) -> Option<(usize, char)> {
        if matches!(iter.peek(), Some((_, ch)) if ch == next) {
            return iter.next();
        }
        return None;
    }

    fn next_if<F>(iter: &mut I, cmp: F) -> Option<(usize, char)>
    where
        F: Fn(&usize, &char) -> bool,
    {
        if matches!(iter.peek(), Some((ind, ch)) if cmp(ind, ch)) {
            return iter.next();
        }
        return None;
    }

    fn consume_if(iter: &mut I, next: &char) -> bool {
        Token::next_if_matches(iter, next).is_some()
    }

    fn consume_comment(iter: &mut I) {
        while Token::next_if(iter, |_, ch| ch != &'\n').is_some() {}
    }

    fn consume_number(iter: &mut I, start: char) -> Result<f64, TokenParseError> {
        let mut text = String::from(start);

        Token::consume_digits(iter, &mut text);

        // reset peek to check for decimal
        iter.reset_peek();
        if matches!(iter.peek(), Some((_, ch)) if ch == &'.')
            && matches!(iter.peek(), Some((_, ch)) if ch.is_digit(10))
        {
            println!("HERE");
            // consume '.' and the following digits
            iter.next();
            text.push('.');
            Token::consume_digits(iter, &mut text);
        }

        Ok(text.parse()?)
    }

    fn consume_digits(iter: &mut I, text: &mut String) {
        while let Some((_ind, digit)) = Token::next_if(iter, |_ind, ch| ch.is_digit(10)) {
            text.push(digit);
        }
    }

    fn consume_string(iter: &mut I, start: usize) -> Result<String, TokenParseError> {
        let mut text = String::new();
        while let Some((_ind, ch)) = iter.next() {
            match ch {
                '"' => return Ok(text),
                ch => text.push(ch),
            }
        }

        // if we are here, we have read the whole iterator and never reached the closing tag for
        // the string
        Err(TokenParseError::UnterminatedString(start))
    }

    fn consume_identifier(iter: &mut I, start: char) -> TokenType {
        let mut text = String::from(start);

        while let Some((_, ch)) = Token::next_if(iter, |_, ch| ch.is_alphanumeric()) {
            text.push(ch);
        }

        match text.as_str() {
            "class" => TokenType::Class,
            "else" => TokenType::Else,
            "false" => TokenType::False,
            "for" => TokenType::For,
            "fn" => TokenType::Fn,
            "if" => TokenType::If,
            "nil" => TokenType::Nil,
            "print" => TokenType::Print,
            "return" => TokenType::Return,
            "super" => TokenType::Super,
            "this" => TokenType::This,
            "true" => TokenType::True,
            "let" => TokenType::Let,
            "while" => TokenType::While,
            _ => TokenType::Identifier(text),
        }
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use super::*;

    fn get_iter(content: &'static str) -> I<'static> {
        content.chars().enumerate().multipeek()
    }

    #[test]
    fn next_if_matches_true_returns_next() {
        let mut iter = get_iter("=");
        let res = Token::next_if_matches(&mut iter, &'=');
        assert!(res.is_some(), "Unexpected empty result");
        assert_eq!(res.unwrap(), (0, '='), "Did not return expected element");
        assert!(iter.next().is_none(), "Did not consume next");
    }

    #[test]
    fn next_if_matches_false_return_none() {
        let mut iter = get_iter("=");
        let res = Token::next_if_matches(&mut iter, &'!');
        assert!(res.is_none(), "Unexpected result");
        assert!(iter.next().is_some(), "Called next on iter");
    }

    #[test]
    fn consume_string_returns_content() -> Result<(), TokenParseError> {
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
                TokenParseError::UnterminatedString(ind) => ind == start,
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
    fn consume_number() -> Result<(), TokenParseError> {
        // the first digit is consumed in the iterator
        let mut iter = get_iter("23123");
        let num = Token::consume_number(&mut iter, '1')?;
        assert_eq!(num, 123123.0);
        Ok(())
    }

    #[test]
    fn consume_decimal_number() -> Result<(), TokenParseError> {
        // the first digit is consumed in the iterator
        let mut iter = get_iter("23.123");
        let num = Token::consume_number(&mut iter, '1')?;
        assert_eq!(num, 123.123);
        Ok(())
    }

    #[test]
    fn consume_identifier_ignores_space() {
        let mut iter = get_iter("a a");
        let res = Token::consume_identifier(&mut iter, 'a');
        assert_eq!(res, TokenType::Identifier("aa".to_string()));
    }

    // from tests
    #[test]
    fn from_left_paren() -> Result<(), TokenParseError> {
        let res = Token::from(&mut get_iter("("))?;
        assert_eq!(res, Some(TokenType::LeftParen));
        Ok(())
    }

    #[test]
    fn from_right_paren() -> Result<(), TokenParseError> {
        let res = Token::from(&mut get_iter(")"))?;
        assert_eq!(res, Some(TokenType::RightParen));
        Ok(())
    }

    #[test]
    fn from_left_brace() -> Result<(), TokenParseError> {
        let res = Token::from(&mut get_iter("{"))?;
        assert_eq!(res, Some(TokenType::LeftBrace));
        Ok(())
    }

    #[test]
    fn from_right_brace() -> Result<(), TokenParseError> {
        let res = Token::from(&mut get_iter("}"))?;
        assert_eq!(res, Some(TokenType::RightBrace));
        Ok(())
    }

    #[test]
    fn from_comma() -> Result<(), TokenParseError> {
        let res = Token::from(&mut get_iter(","))?;
        assert_eq!(res, Some(TokenType::Comma));
        Ok(())
    }

    #[test]
    fn from_dot() -> Result<(), TokenParseError> {
        let res = Token::from(&mut get_iter("."))?;
        assert_eq!(res, Some(TokenType::Dot));
        Ok(())
    }

    #[test]
    fn from_minus() -> Result<(), TokenParseError> {
        let res = Token::from(&mut get_iter("-"))?;
        assert_eq!(res, Some(TokenType::Minus));
        Ok(())
    }

    #[test]
    fn from_plus() -> Result<(), TokenParseError> {
        let res = Token::from(&mut get_iter("+"))?;
        assert_eq!(res, Some(TokenType::Plus));
        Ok(())
    }

    #[test]
    fn from_semicolon() -> Result<(), TokenParseError> {
        let res = Token::from(&mut get_iter(";"))?;
        assert_eq!(res, Some(TokenType::Semicolon));
        Ok(())
    }

    #[test]
    fn from_star() -> Result<(), TokenParseError> {
        let res = Token::from(&mut get_iter("*"))?;
        assert_eq!(res, Some(TokenType::Star));
        Ok(())
    }

    #[test]
    fn from_bang_equal() -> Result<(), TokenParseError> {
        let res = Token::from(&mut get_iter("!="))?;
        assert_eq!(res, Some(TokenType::BangEqual));
        Ok(())
    }

    #[test]
    fn from_bang() -> Result<(), TokenParseError> {
        let res = Token::from(&mut get_iter("!"))?;
        assert_eq!(res, Some(TokenType::Bang));
        Ok(())
    }

    #[test]
    fn from_equal_equal() -> Result<(), TokenParseError> {
        let res = Token::from(&mut get_iter("=="))?;
        assert_eq!(res, Some(TokenType::EqualEqual));
        Ok(())
    }

    #[test]
    fn from_equal() -> Result<(), TokenParseError> {
        let res = Token::from(&mut get_iter("="))?;
        assert_eq!(res, Some(TokenType::Equal));
        Ok(())
    }

    #[test]
    fn from_less_equal() -> Result<(), TokenParseError> {
        let res = Token::from(&mut get_iter("<="))?;
        assert_eq!(res, Some(TokenType::LessEqual));
        Ok(())
    }

    #[test]
    fn from_less() -> Result<(), TokenParseError> {
        let res = Token::from(&mut get_iter("<"))?;
        assert_eq!(res, Some(TokenType::Less));
        Ok(())
    }

    #[test]
    fn from_greater_equal() -> Result<(), TokenParseError> {
        let res = Token::from(&mut get_iter(">="))?;
        assert_eq!(res, Some(TokenType::GreaterEqual));
        Ok(())
    }

    #[test]
    fn from_greater() -> Result<(), TokenParseError> {
        let res = Token::from(&mut get_iter(">"))?;
        dbg!(&res);
        assert_eq!(res, Some(TokenType::Greater));
        Ok(())
    }

    #[test]
    fn from_comment() -> Result<(), TokenParseError> {
        let res = Token::from(&mut get_iter("//"))?;
        assert_eq!(res, None);
        Ok(())
    }

    #[test]
    fn from_slash() -> Result<(), TokenParseError> {
        let res = Token::from(&mut get_iter("/"))?;
        assert_eq!(res, Some(TokenType::Slash));
        Ok(())
    }

    #[test]
    fn from_string() -> Result<(), TokenParseError> {
        let res = Token::from(&mut get_iter("\"test\""))?;
        assert_eq!(res, Some(TokenType::String("test".to_string())));
        Ok(())
    }

    #[test]
    fn from_space() -> Result<(), TokenParseError> {
        let res = Token::from(&mut get_iter(" "))?;
        assert_eq!(res, None);
        Ok(())
    }

    #[test]
    fn from_number() -> Result<(), TokenParseError> {
        let res = Token::from(&mut get_iter("123.123"))?;
        assert_eq!(res, Some(TokenType::Number(123.123)));
        Ok(())
    }

    #[test]
    fn from_return() -> Result<(), TokenParseError> {
        let res = Token::from(&mut get_iter("\r"))?;
        assert_eq!(res, None);
        Ok(())
    }

    #[test]
    fn from_tab() -> Result<(), TokenParseError> {
        let res = Token::from(&mut get_iter("\t"))?;
        assert_eq!(res, None);
        Ok(())
    }

    #[test]
    fn from_new_line() -> Result<(), TokenParseError> {
        let res = Token::from(&mut get_iter("\n"))?;
        assert_eq!(res, None);
        Ok(())
    }

    #[test]
    fn from_or() -> Result<(), TokenParseError> {
        let res = Token::from(&mut get_iter("||"))?;
        assert_eq!(res, Some(TokenType::Or));
        Ok(())
    }

    #[test]
    fn from_and() -> Result<(), TokenParseError> {
        let res = Token::from(&mut get_iter("&&"))?;
        assert_eq!(res, Some(TokenType::And));
        Ok(())
    }

    #[test]
    fn from_identifier() -> Result<(), TokenParseError> {
        let res = Token::from(&mut get_iter("aa bb"))?;
        assert_eq!(res, Some(TokenType::Identifier("aa".to_string())));
        Ok(())
    }

    // TODO: Test reserved words
}
