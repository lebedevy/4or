use itertools::Itertools;

use crate::token::Token;

use crate::scanner::token::ReadFrom;

mod token;

pub struct Scanner {
    // TODO: Clean up
    content: String,
    start: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    pub fn new(content: String) -> Self {
        Self {
            content,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    // TODO: return a result
    pub fn scan_tokens(&mut self) -> Vec<Token> {
        let mut tokens = vec![];
        let mut iter = self.content.chars().enumerate().multipeek();
        while let Some(_) = iter.peek() {
            self.start = self.current; // TODO: Clean up
            let token = Token::read_next(&mut iter);

            match token {
                Ok(token) => {
                    // If some token exist, add it
                    if let Some(token) = token {
                        tokens.push(token);
                    }
                }
                Err(err) => eprintln!("{:?}", err),
            }
        }

        tokens
    }
}
