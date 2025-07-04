pub(crate) use token_type::TokenType;

mod token_type;

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Token {
    pub(crate) token_type: TokenType,
    pub(crate) index: usize,
}
