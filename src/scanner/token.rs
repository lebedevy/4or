use core::fmt;
use std::{
    iter::{Enumerate, Peekable},
    str::Chars,
};

#[derive(Debug)]
enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String(String),
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fn,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Let,
    While,

    EOF,
}

pub(super) enum TokenError {
    TokenTypeError(TokenTypeError),
}

#[derive(Debug)]
pub(crate) struct Token {
    token_type: TokenType,
}

type I<'a> = Peekable<Enumerate<Chars<'a>>>;

impl Token {
    pub(super) fn read_next(iter: &mut I) -> Result<Option<Self>, TokenError> {
        match TokenType::from(iter) {
            Ok(token) => match token {
                Some(token_type) => Ok(Some(Self { token_type })),
                None => Ok(None),
            },
            Err(err) => Err(TokenError::TokenTypeError(err)),
        }
    }
}

#[derive(Debug, Clone)]
enum TokenTypeError {
    InvalidToken(char, usize),
    EmptyIterator,
    UnterminatedString(usize),
}

impl fmt::Display for TokenTypeError {
    // TODO: Write out the offending character
    fn fmt<'a>(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TokenTypeError::InvalidToken(char, ind) =>
                    format!("Token {} at postion {} is not a valid token", char, ind),
                TokenTypeError::EmptyIterator => "Attempting to read empty iterator".into(),
                TokenTypeError::UnterminatedString(ind) =>
                    format!("String starting at {} is not terminated", ind),
            }
        )
    }
}

impl TokenType {
    fn from(iter: &mut I) -> Result<Option<TokenType>, TokenTypeError> {
        let token = match iter.next() {
            Some((ind, c)) => match c {
                '(' => Some(Self::LeftParen),
                ')' => Some(Self::RightParen),
                '{' => Some(Self::LeftBrace),
                '}' => Some(Self::RightBrace),
                ',' => Some(Self::Comma),
                '.' => Some(Self::Dot),
                '-' => Some(Self::Minus),
                '+' => Some(Self::Plus),
                ';' => Some(Self::Semicolon),
                '*' => Some(Self::Star),
                // Potentially compound tokens
                '!' if TokenType::match_next_token(iter, &'=') => Some(Self::BangEqual),
                '!' => Some(Self::Bang),
                '=' if TokenType::match_next_token(iter, &'=') => Some(Self::EqualEqual),
                '=' => Some(Self::Equal),
                '<' if TokenType::match_next_token(iter, &'=') => Some(Self::LessEqual),
                '<' => Some(Self::Less),
                '>' if TokenType::match_next_token(iter, &'=') => Some(Self::GreaterEqual),
                '>' => Some(Self::Greater),
                // division or comment
                '/' if TokenType::match_next_token(iter, &'/') => {
                    TokenType::consume_comment(iter);
                    None
                }
                '/' => Some(Self::Slash),
                // Strings
                '"' => Some(Self::String(TokenType::consume_string(iter, ind)?)),
                // ignored chars
                // TODO: Increment on line on new line
                ' ' | '\r' | '\t' | '\n' => None,
                _ => {
                    return Err(TokenTypeError::InvalidToken(c, ind));
                }
            },
            None => {
                return Err(TokenTypeError::EmptyIterator);
            }
        };

        Ok(token)
    }

    fn match_next_token(iter: &mut I, next: &char) -> bool {
        match iter.peek() {
            Some((_ind, ch)) => ch == next,
            None => false,
        }
    }

    fn consume_comment(iter: &mut I) {
        while let Some((_ind, ch)) = iter.next() {
            // compound conditionals with let are unstable; do this check inside the body instead
            if ch == '\n' {
                break;
            };
        }
    }

    fn consume_string(iter: &mut I, start: usize) -> Result<String, TokenTypeError> {
        let mut text = String::new();
        while let Some((_ind, ch)) = iter.next() {
            match ch {
                '"' => return Ok(text),
                ch => text.push(ch),
            }
        }

        // if we are here, we have read the whole iterator and never reached the closing tag for
        // the string
        Err(TokenTypeError::UnterminatedString(start))
    }
}
