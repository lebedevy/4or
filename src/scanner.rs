use token::Token;

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
        let mut iter = self.content.chars().peekable().enumerate();
        loop {
            self.start = self.current; // TODO: Clean up
            let token = Token::read_next(&mut iter);
            match token {
                Ok(token) => tokens.push(token),
                Err(_) => todo!(),
            }
            break;
        }

        tokens
    }
}
