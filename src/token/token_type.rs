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
    // TODO: Move this to a library and out of the core language
    #[deprecated()]
    Print,
    Return,
    Super,
    This,
    True,
    Let,
    While,

    EOF,
}
