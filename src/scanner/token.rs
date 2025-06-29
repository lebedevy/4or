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
    String,
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

type I<'a> = Enumerate<Peekable<Chars<'a>>>;

impl Token {
    pub(super) fn read_next(iter: &mut I) -> Result<Self, TokenError> {
        match TokenType::from(iter) {
            Ok(token_type) => Ok(Self { token_type }),
            Err(err) => Err(TokenError::TokenTypeError(err)),
        }
    }
}

#[derive(Debug, Clone)]
enum TokenTypeError {
    InvalidToken(char, usize),
    EmptyIterator,
}

impl fmt::Display for TokenTypeError {
    // TODO: Write out the offending character
    fn fmt<'a>(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Not a valid token")
    }
}

impl TokenType {
    fn from(c: &mut I) -> Result<TokenType, TokenTypeError> {
        match c.next() {
            Some((ind, c)) => match c {
                '(' => Ok(Self::LeftParen),
                ')' => Ok(Self::RightParen),
                '{' => Ok(Self::LeftBrace),
                '}' => Ok(Self::RightBrace),
                ',' => Ok(Self::Comma),
                '.' => Ok(Self::Dot),
                '-' => Ok(Self::Minus),
                '+' => Ok(Self::Plus),
                ';' => Ok(Self::Semicolon),
                '*' => Ok(Self::Star),
                _ => Err(TokenTypeError::InvalidToken(c, ind)),
            },
            None => Err(TokenTypeError::EmptyIterator),
        }
    }
}
