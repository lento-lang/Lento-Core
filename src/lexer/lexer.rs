use std::{io::{Seek, BufRead}, collections::HashMap};

use lazy_regex::{self, regex_replace_all};

use super::{token::{Token, LineInfoSpan, TokenInfo}, error::LexerError, op::Operator};

pub type LexResult = Result<TokenInfo, LexerError>;
const BUFFER_SIZE: usize = 128;


/**
 * The lexer tokenize and output a stream of tokens that are generated from the source code.
 * The lexer is a state machine that is used by the parser to generate an AST.
 */
#[derive(Clone)]
pub struct Lexer<R: BufRead + Seek> {
    reader: R,
    initialized_buffer: bool,
    buffer: [u8; BUFFER_SIZE],
    buffer_idx: usize,
    line_info: LineInfoSpan,
    operators: HashMap<String, Operator>,
    peeked_token: Option<LexResult>,
}

impl<R: BufRead + Seek> Lexer<R> {
    pub fn new(reader: R) -> Self {
        Self {
            reader,
            initialized_buffer: false,
            buffer: [0; BUFFER_SIZE],
            buffer_idx: 0,
            line_info: LineInfoSpan::new(),
            operators: HashMap::new(),
            peeked_token: None,
        }
    }

    pub fn define_op(&mut self, op: Operator) {
        self.operators.insert(op.symbol().clone(), op);
    }

    fn lookup_op(&self, op: &str) -> Option<Operator> {
        // TODO: Incremental search using peek_char and break when there is no match,
        //      otherwise, return the longest match while peeking all the way to the end and "eating" the characters if there is a match.
        self.operators.get(op).cloned()
    }


    fn new_token_info(&self, token: Token) -> LexResult {
        Ok(TokenInfo {
            info: self.line_info.clone(),
            token
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
            let c = self.buffer[idx as usize];
            if c == 0 { None } else { Some(c as char) }
        } else {
            // Read from reader
            let prev_idx = self.reader.seek(std::io::SeekFrom::Current(0)).unwrap();
            self.reader.seek(std::io::SeekFrom::Current(offset)).unwrap();
            let c = &mut [0; 1];
            if let Ok(n) = self.reader.read(c) {
                self.reader.seek(std::io::SeekFrom::Start(prev_idx)).unwrap();
                if n > 0 { Some(c[0] as char) } else { None }
            } else {
                self.reader.seek(std::io::SeekFrom::Start(prev_idx)).unwrap();
                None
            }
        }
    }

    /**
     * Read a string from the source code using a buffered reader.
     */
    fn next_char(&mut self) -> Option<char> {
        if !self.initialized_buffer || self.buffer_idx >= BUFFER_SIZE {
            if self.reader.read(&mut self.buffer).is_err() {
                // Try to re-initialize the buffer the next time
                return None;
            }
            self.initialized_buffer = true;
            self.buffer_idx = 0;
        }
        let c = self.buffer[self.buffer_idx];
        if c == 0 { return None; }
        self.buffer_idx += 1;
        self.line_info.end.column += 1;
        Some(c as char)
    }

    /**
     * Peek the next token from the source code.
     */
    pub fn peek_token(&mut self) -> LexResult {
        if self.peeked_token.is_some() { self.peeked_token.as_ref().unwrap().clone() }
        else {
            let token = self.next_token();
            self.peeked_token = Some(token.clone());
            token
        }
    }

    /**
     * Get the next token from the source code, ignoring newlines.
     */
    pub fn next_token_no_nl(&mut self) -> LexResult {
        let mut token = self.next_token()?;
        while token.token == Token::Newline {
            token = self.next_token()?;
        }
        Ok(token)
    }

    /**
     * Get the next token from the source code.
     * This function is the main function of the lexer.
     */
    pub fn next_token(&mut self) -> LexResult {
        if let Some(token) = self.peeked_token.take() { return token; }
        self.set_info_start_to_current();
        if let Some(c) = self.next_char() {
            if c == ' ' || c == '\t' || c == '\r' {
                return self.next_token(); // Ignore whitespace
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
            } else { self.new_token_info(match c {
                '(' => Token::LeftParen,
                ')' => Token::RightParen,
                '{' => Token::LeftBrace,
                '}' => Token::RightBrace,
                '[' => Token::LeftBracket,
                ']' => Token::RightBracket,
                _ => {
                    if let Some(op) = self.lookup_op(&c.to_string()) { Token::Op(op) }
                    else { return Err(LexerError::unexpected_character(c, self.line_info.clone())); }
                }
            }) }
        } else { self.new_token_info(Token::EndOfFile) }
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
    fn read_while(&mut self, init: Option<String>, mut cond: impl FnMut(char) -> bool, mut build_token: impl FnMut(&mut Self, String) -> LexResult) -> LexResult {
        let mut r = init.unwrap_or(String::new());
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
        self.read_while(None, |c| c != '"', |this, s| {
            this.next_char(); // Eat the last "
            this.new_token_info(Token::String(Lexer::<R>::resolve_escape_sequence(s)))
        })
    }


    /**
     * Read a character from the source code.
     */
    fn read_char(&mut self) -> LexResult {
        self.read_while(None, |c| c != '\'', |this, s| {
            let s = Lexer::<R>::resolve_escape_sequence(s);
            if s.len() != 1 {
                Err(LexerError::invalid_char(s, this.line_info.clone()))
            } else {
                this.new_token_info(Token::Char(s.chars().next().unwrap()))
            }
        })
    }

    /**
     * Read a number from the source code.
     * Can be an integer or a float (casted at runtime).
     */
    fn read_number(&mut self, c: char) -> LexResult {
        let mut has_dot = false;
        self.read_while(Some(c.to_string()), move |c| {
            if c == '.' && !has_dot {
                has_dot = true;
                true
            } else {
                c.is_numeric()
            }
        }, |this, s| this.new_token_info(if has_dot { Token::Float(s) } else { Token::Integer(s) }))
    }

    fn is_identifier_head_char(c: char) -> bool { c.is_alphabetic() || c == '_' }
    fn is_identifier_body_char(c: char) -> bool { c.is_alphanumeric() || c == '_' }

    /**
     * Read an identifier from the source code.
     */
    fn read_identifier(&mut self, c: char) -> LexResult {
        self.read_while(Some(c.to_string()), Self::is_identifier_body_char, |this, s| this.new_token_info(Token::Identifier(s)))
    }

}
