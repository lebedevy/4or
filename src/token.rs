use std::fmt::Display;

pub(crate) use token_type::TokenType;

mod token_type;

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Token {
    pub(crate) token_type: TokenType,
    pub(crate) index: usize,
}

impl Token {
    pub(crate) fn new(token_type: TokenType, index: usize) -> Self {
        Self { token_type, index }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "column - {}; token - {};", self.index, self.token_type)?;

        Ok(())
    }
}
