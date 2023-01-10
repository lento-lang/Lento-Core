use std::io::{Seek, BufRead};

use super::{token::Token, error::LexerError};

#[derive(Clone)]
pub struct Lexer<R> {
    pub reader: R,
    pub index: usize,
    pub prev_tokens: Vec<Token>,
}

impl<R: BufRead + Seek> Lexer<R> {
    pub fn new(reader: R) -> Self {
        Self {
            reader,
            index: 0,
            prev_tokens: Vec::new()
        }
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        // let mut buffer = [0; 1];
        // self.reader.read_exact(&mut buffer)?;
        // let token = Token::from_char(buffer[0] as char);
        // self.prev_tokens.push(token.clone());
        // self.index += 1;
        // Ok(token)
        Ok(Token::EndOfFile)
    }
}
