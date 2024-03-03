use std::{
    collections::HashMap,
    io::{Read, Seek},
};

use lazy_regex::{self, regex_replace_all};

use crate::{
    interpreter::error::{runtime_error, RuntimeError},
    util::failable::Failable,
};

use super::{
    error::LexerError,
    op::Operator,
    token::{LineInfoSpan, Token, TokenInfo},
};

//--------------------------------------------------------------------------------------//
//                                        Lexer                                         //
//--------------------------------------------------------------------------------------//

pub type LexResult = Result<TokenInfo, LexerError>;
const BUFFER_SIZE: usize = 128;

/**
 * The lexer tokenize and output a stream of tokens that are generated from the source code.
 * The lexer is a state machine that is used by the parser to generate an AST.
 */
#[derive(Clone)]
pub struct Lexer<R>
where
    R: Read + Seek,
{
    reader: R,
    is_stream: bool,
    should_read: bool,
    buffer: [u8; BUFFER_SIZE],
    buffer_idx: usize,
    line_info: LineInfoSpan,
    operators: HashMap<String, Operator>,
    peeked_tokens: Vec<LexResult>, // Queue of peeked tokens (FIFO)
}

impl<R: Read + Seek> Lexer<R> {
    /**
     * Create a new lexer from a reader.
     * The lexer will read from the reader until it reaches the end of the file.
     */
    pub fn new(reader: R) -> Self {
        Self {
            reader,
            is_stream: false,
            should_read: true,
            buffer: [0; BUFFER_SIZE],
            buffer_idx: 0,
            line_info: LineInfoSpan::new(),
            operators: HashMap::new(),
            peeked_tokens: Vec::new(),
        }
    }

    /**
     * Create a new lexer from a reader.
     * The lexer will read from the reader until it reaches the end of the stream, then the parser must call `refill_on_read` to refill the buffer.
     * If the reader is a stream, the lexer will be able to refill the buffer with new data from the reader on the next read (at any time)
     */
    pub fn new_stream(reader: R) -> Self {
        Self {
            reader,
            is_stream: true,
            should_read: true,
            buffer: [0; BUFFER_SIZE],
            buffer_idx: 0,
            line_info: LineInfoSpan::new(),
            operators: HashMap::new(),
            peeked_tokens: Vec::new(),
        }
    }

    pub fn get_reader(&mut self) -> &mut R {
        &mut self.reader
    }

    pub fn reset(&mut self) {
        self.should_read = true;
        self.buffer = [0; BUFFER_SIZE];
        self.buffer_idx = 0;
        self.line_info = LineInfoSpan::new();
        self.peeked_tokens.clear();
    }

    pub fn define_op(&mut self, op: Operator) -> Failable<RuntimeError> {
        if self.operators.contains_key(&op.symbol()) {
            return Err(runtime_error(format!(
                "Cannot override operator '{}'",
                op.symbol()
            )));
        }
        self.operators.insert(op.symbol().clone(), op);
        Ok(())
    }

    pub fn lookup_op(&self, op: &str) -> Option<Operator> {
        // TODO: Incremental search using peek_char and break when there is no match,
        //      otherwise, return the longest match while peeking all the way to the end and "eating" the characters if there is a match.
        self.operators.get(op).cloned()
    }

    fn new_token_info(&self, token: Token) -> LexResult {
        Ok(TokenInfo {
            info: self.line_info.clone(),
            token,
        })
    }

    /**
     * Set the start of the token to the current position of the lexer.
     * This function is called when the lexer is about to start reading a new token.
     */
    fn set_info_start_to_current(&mut self) {
        self.line_info.start = self.line_info.end.clone();
    }

    fn peek_char(&mut self, offset: i64) -> Option<char> {
        // Read from buffer
        let idx = self.buffer_idx + offset as usize;
        if idx < BUFFER_SIZE {
            let c = self.buffer[idx];
            if c == 0 {
                None
            } else {
                Some(c as char)
            }
        } else {
            // Read from reader
            let prev_idx = self.reader.stream_position().unwrap();
            self.reader
                .seek(std::io::SeekFrom::Current(offset))
                .unwrap();
            let c = &mut [0; 1];
            if let Ok(n) = self.reader.read(c) {
                self.reader
                    .seek(std::io::SeekFrom::Start(prev_idx))
                    .unwrap();
                if n > 0 {
                    Some(c[0] as char)
                } else {
                    None
                }
            } else {
                self.reader
                    .seek(std::io::SeekFrom::Start(prev_idx))
                    .unwrap();
                None
            }
        }
    }

    /**
     * Read a string from the source code using a buffered reader.
     */
    fn next_char(&mut self) -> Option<char> {
        if self.should_read || self.buffer_idx >= BUFFER_SIZE {
            match self.reader.read(&mut self.buffer) {
                Ok(n) => {
                    if n == 0 {
                        return None;
                    }
                    self.should_read = false;
                    self.buffer_idx = 0;
                }
                Err(_) => {
                    // Try to re-initialize the buffer the next time
                    return None;
                }
            }
        }
        let c = self.buffer[self.buffer_idx];
        if c == 0 {
            return None;
        }
        self.buffer_idx += 1;
        self.line_info.end.column += 1;
        Some(c as char)
    }

    fn get_peeked_token(&mut self, offset: usize) -> Option<LexResult> {
        if self.peeked_tokens.is_empty() || offset >= self.peeked_tokens.len() {
            None
        } else {
            Some(self.peeked_tokens[offset].clone())
        }
    }

    fn consume_peeked_token(&mut self, offset: usize) -> Option<LexResult> {
        if self.peeked_tokens.is_empty() || offset >= self.peeked_tokens.len() {
            None
        } else {
            Some(self.peeked_tokens.remove(offset))
        }
    }

    /**
     * Peek the next token from the source code.
     * ! Note: This function should not be called from read_next_token() because it will cause an infinite loop (stack overflow)
     */
    pub fn peek_token(&mut self, offset: usize) -> LexResult {
        if let Some(t) = self.get_peeked_token(offset) {
            t
        } else {
            let token = self.next_token();
            // Do not push EOF to the peeked tokens list
            if let Ok(TokenInfo {
                token: Token::EndOfFile,
                ..
            }) = token
            {
                return token;
            }
            self.peeked_tokens.push(token.to_owned());
            self.peek_token(offset)
        }
    }

    pub fn peek_token_no_nl(&mut self) -> LexResult {
        let mut idx = 0usize;
        let mut token = self.peek_token(idx);
        while let Ok(TokenInfo {
            token: Token::Newline,
            ..
        }) = token
        {
            token = self.peek_token(idx + 1);
            idx += 1;
        }
        token
    }

    /**
     * Get the next token from the source code, ignoring newlines.
     * Or return the next peeked token (again, ignoring newlines)
     */
    pub fn read_next_token_no_nl(&mut self) -> LexResult {
        let mut token = self.read_next_token();
        while let Ok(TokenInfo {
            token: Token::Newline,
            ..
        }) = token
        {
            token = self.read_next_token();
        }
        token
    }

    /**
     * Get the next token from the source code,
     * Or return the next peeked token.
     */
    pub fn read_next_token(&mut self) -> LexResult {
        if let Some(token) = self.consume_peeked_token(0) {
            token
        } else {
            self.next_token()
        }
    }

    /**
     * Refill the buffer with new data from the reader on the next read.
     * This should only be called from the parser when the lexer has reached an unexpected end of file.
     * Usually only used in REPL mode.
     */
    pub fn refill_on_read(&mut self) {
        // If the reader is a stream, we can refill the buffer on the next read
        self.should_read = self.is_stream;
    }

    /**
     * Expect there to be a token (not EOF).
     * If there is a token, return it.
     * Else, try to read from the source if the reader is a stream.
     * Then return the first read token (can be EOF but will throw error later)
     */
    fn expect_next_token(&mut self) -> LexResult {
        let token = self.read_next_token()?;
        if token.token == Token::EndOfFile {
            self.should_read = self.is_stream;
            self.read_next_token()
        } else {
            Ok(token)
        }
    }

    /**
     * Expect there to be a token (not EOF) while ignoring newlines.
     * If there is a token, return it.
     * Else, try to read from the source if the reader is a stream.
     * Then return the first read token (can be EOF but will throw error later)
     * Or return the next peeked token (again, ignoring newlines)
     *
     * Direct copy of `read_next_token_no_nl` but expecting tokens from the stream.
     */
    pub fn expect_next_token_no_nl(&mut self) -> LexResult {
        let mut token = self.expect_next_token();
        while let Ok(TokenInfo {
            token: Token::Newline,
            ..
        }) = token
        {
            token = self.expect_next_token();
        }
        token
    }

    /**
     * Get the next token from the source code.
     * This function is the main function of the lexer.
     * This function can only move forward, it cannot go back.
     */
    fn next_token(&mut self) -> LexResult {
        self.set_info_start_to_current();
        if let Some(c) = self.next_char() {
            if c == ' ' || c == '\t' || c == '\r' {
                self.next_token() // Ignore whitespace
            } else if c == '\n' {
                self.line_info.end.line += 1;
                self.line_info.end.column = 1;
                self.new_token_info(Token::Newline)
            } else if c == '"' {
                self.read_string()
            } else if c == '\'' {
                self.read_char()
            } else if c.is_numeric() {
                self.read_number(c)
            } else if Self::is_identifier_head_char(c) {
                self.read_identifier(c)
            } else {
                self.new_token_info(match c {
                    '(' => Token::LeftParen,
                    ')' => Token::RightParen,
                    '{' => Token::LeftBrace,
                    '}' => Token::RightBrace,
                    '[' => Token::LeftBracket,
                    ']' => Token::RightBracket,
                    ',' => Token::Comma,
                    ';' => Token::SemiColon,
                    _ => {
                        if let Some(op) = self.lookup_op(&c.to_string()) {
                            Token::Op(op)
                        } else {
                            return Err(LexerError::unexpected_character(
                                c,
                                self.line_info.clone(),
                            ));
                        }
                    }
                })
            }
        } else {
            self.new_token_info(Token::EndOfFile)
        }
    }

    pub fn resolve_escape_sequence(s: String) -> String {
        let s = regex_replace_all!(r#"\\u(\d{4})"#, &s, |_, num: &str| {
            let num: u32 = u32::from_str_radix(num, 16).unwrap();
            let c: char = std::char::from_u32(num).unwrap();
            c.to_string()
        });
        let s = regex_replace_all!(r#"\\x(\d{2})"#, &s, |_, num: &str| {
            let num: u32 = u32::from_str_radix(num, 16).unwrap();
            let c: char = std::char::from_u32(num).unwrap();
            c.to_string()
        });
        s.to_string()
    }

    /**
     * Helper function to read a token from the source code using a predicates and lambdas or function composition.
     */
    fn read_while(
        &mut self,
        init: Option<String>,
        mut cond: impl FnMut(char) -> bool,
        mut build_token: impl FnMut(&mut Self, String) -> LexResult,
    ) -> LexResult {
        let mut r = init.unwrap_or_default();
        while let Some(c) = self.peek_char(0) {
            if cond(c) {
                self.next_char();
                r.push(c);
            } else {
                // We have reached the end of the identifier
                return build_token(self, r);
            }
        }
        Err(LexerError::unexpected_end_of_file(self.line_info.clone()))
    }

    /**
     * Read a string from the source code.
     */
    fn read_string(&mut self) -> LexResult {
        self.read_while(
            None,
            |c| c != '"',
            |this, s| {
                this.next_char(); // Eat the last "
                this.new_token_info(Token::String(Lexer::<R>::resolve_escape_sequence(s)))
            },
        )
    }

    /**
     * Read a character from the source code.
     */
    fn read_char(&mut self) -> LexResult {
        let c = self.read_while(
            None,
            |c| c != '\'',
            |this, s| {
                let s = Lexer::<R>::resolve_escape_sequence(s);
                if s.len() != 1 {
                    Err(LexerError::invalid_char(s, this.line_info.clone()))
                } else {
                    this.new_token_info(Token::Char(s.chars().next().unwrap()))
                }
            },
        );
        self.next_char(); // Eat the last '
        c
    }

    /**
     * Read a number from the source code.
     * Can be an integer or a float (casted at runtime).
     */
    fn read_number(&mut self, c: char) -> LexResult {
        let mut has_dot = false;
        self.read_while(
            Some(c.to_string()),
            move |c| {
                if c == '.' && !has_dot {
                    has_dot = true;
                    true
                } else {
                    c.is_numeric()
                }
            },
            |this, s| {
                this.new_token_info(if has_dot {
                    Token::Float(s)
                } else {
                    Token::Integer(s)
                })
            },
        )
    }

    fn is_identifier_head_char(c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }
    fn is_identifier_body_char(c: char) -> bool {
        c.is_alphanumeric() || c == '_'
    }

    /**
     * Read an identifier from the source code.
     */
    fn read_identifier(&mut self, c: char) -> LexResult {
        self.read_while(
            Some(c.to_string()),
            Self::is_identifier_body_char,
            |this, s| this.new_token_info(Token::Identifier(s)),
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::stdlib::init::init_lexer;

    use super::*;
    use std::io::Cursor;

    #[test]
    fn test_lexer_assign() {
        let mut lexer = Lexer::new(Cursor::new("x = 1;"));
        init_lexer(&mut lexer);
        let token = lexer.read_next_token().unwrap();
        assert_eq!(token.token, Token::Identifier("x".to_string())); // x
        let token = lexer.read_next_token().unwrap();
        assert!(matches!(token.token, Token::Op(Operator::Static(_)))); // =
        let token = lexer.read_next_token().unwrap();
        assert_eq!(token.token, Token::Integer("1".to_string())); // 1
        let token = lexer.read_next_token().unwrap();
        assert_eq!(token.token, Token::SemiColon); // ;
        let token = lexer.read_next_token().unwrap();
        assert_eq!(token.token, Token::EndOfFile); // EOF
    }
}
