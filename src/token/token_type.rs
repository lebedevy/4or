use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TokenType {
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
    Identifier(String),
    String(String),
    Number(f64), // this can be optimized with different num types

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
    Return,
    Super,
    This,
    True,
    Let,
    While,

    EOF,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::LeftParen => write!(f, "(")?,
            TokenType::RightParen => write!(f, ")")?,
            TokenType::LeftBrace => write!(f, "{{")?,
            TokenType::RightBrace => write!(f, "}}")?,
            TokenType::Comma => write!(f, ",")?,
            TokenType::Dot => write!(f, ".")?,
            TokenType::Minus => write!(f, "-")?,
            TokenType::Plus => write!(f, "+")?,
            TokenType::Semicolon => write!(f, ";")?,
            TokenType::Slash => write!(f, "/")?,
            TokenType::Star => write!(f, "*")?,
            TokenType::Bang => write!(f, "!")?,
            TokenType::BangEqual => write!(f, "!=")?,
            TokenType::Equal => write!(f, "=")?,
            TokenType::EqualEqual => write!(f, "==")?,
            TokenType::Greater => write!(f, ">")?,
            TokenType::GreaterEqual => write!(f, ">=")?,
            TokenType::Less => write!(f, "<")?,
            TokenType::LessEqual => write!(f, "<=")?,
            TokenType::Identifier(iden) => write!(f, "Identifier - {}", iden)?,
            TokenType::String(str) => write!(f, "String - {}", str)?,
            TokenType::Number(num) => write!(f, "Number - {}", num)?,
            TokenType::And => write!(f, "&&")?,
            TokenType::Class => write!(f, "class")?,
            TokenType::Else => write!(f, "else")?,
            TokenType::False => write!(f, "false")?,
            TokenType::Fn => write!(f, "fn")?,
            TokenType::For => write!(f, "for")?,
            TokenType::If => write!(f, "if")?,
            TokenType::Nil => write!(f, "nil")?,
            TokenType::Or => write!(f, "or")?,
            TokenType::Return => write!(f, "return")?,
            TokenType::Super => write!(f, "super")?,
            TokenType::This => write!(f, "this")?,
            TokenType::True => write!(f, "true")?,
            TokenType::Let => write!(f, "let")?,
            TokenType::While => write!(f, "while")?,
            TokenType::EOF => write!(f, "eof")?,
        };

        Ok(())
    }
}
