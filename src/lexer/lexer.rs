use std::io::{Seek, BufRead};

use lazy_regex::{self, regex_replace_all};

use super::{token::{Token, LineInfoSpan}, error::LexerError};

type LexResult = Result<Token, LexerError>;
const BUFFER_SIZE: usize = 128;

/**
 * The lexer tokenize and output a stream of tokens that are generated from the source code.
 * The lexer is a state machine that is used by the parser to generate an AST.
 */
#[derive(Clone)]
pub struct Lexer<R: BufRead + Seek> {
    reader: R,
    buffer: [u8; BUFFER_SIZE],
    buffer_idx: usize,
    line_info: LineInfoSpan,
}

impl<R: BufRead + Seek> Lexer<R> {
    pub fn new(reader: R) -> Self {
        Self {
            reader,
            buffer: [0; BUFFER_SIZE],
            buffer_idx: 0,
            line_info: LineInfoSpan::new(),
        }
    }

    /**
     * Set the start of the token to the current position of the lexer.
     * This function is called when the lexer is about to start reading a new token.
     */
    fn set_info_start_to_current(&mut self) {
        self.line_info.start = self.line_info.end.clone();
    }

    /**
     * Update the line info of the lexer.
     * The end will always point to the current position of the lexer.
     */
    fn update_line_info(&mut self, c: char) {
        if c == '\n' {
            self.line_info.end.line += 1;
            self.line_info.end.column = 0;
        } else {
            self.line_info.end.column += 1;
        }
    }

    fn peek_char(&mut self) -> Option<char> {
        let prev_idx = self.reader.seek(std::io::SeekFrom::Current(0)).unwrap();
        let one_c_buf = &mut [0; 1];
        if let Err(_) = self.reader.read(one_c_buf) {
            return None;
        }
        self.reader.seek(std::io::SeekFrom::Start(prev_idx)).unwrap(); // Reset the position of the reader
        Some(one_c_buf[0] as char)
    }

    /**
     * Read a string from the source code using a buffered reader.
     */
    fn next_char(&mut self) -> Option<char> {
        if self.buffer_idx >= BUFFER_SIZE {
            self.reader.read(&mut self.buffer);
            self.buffer_idx = 0;
        }
        let c = self.buffer[self.buffer_idx];
        if c == 0 { return None; }
        self.buffer_idx += 1;
        self.update_line_info(c as char);
        Some(c as char)
    }

    /**
     * Get the next token from the source code.
     * This function is the main function of the lexer.
     */
    fn next_token(&mut self) -> LexResult {
        if let Some(c) = self.next_char() {
            if c == ' ' || c == '\t' || c == '\r' {
                self.next_token()
            } else if c == '"' {
                self.read_string()
            } else if c == '\'' {
                self.read_char()
            } else if c.is_numeric() {
                self.read_number(c)
            } else if c.is_alphabetic() {
                self.read_identifier(c)
            } else {
                Err(LexerError::UnexpectedCharacter(c, self.line_info.clone()))
            }
        } else {
            Ok(Token::EndOfFile)
        }
    }

    fn resolve_escape_sequence(&mut self, s: String) -> String {
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
     * Read a string from the source code.
     */
    fn read_string(&mut self) -> LexResult {
        let mut r = String::new();
        while let Some(c) = self.peek_char() {
            if c == '"' {
                self.next_char();
                return Ok(Token::String(r));
            } else {
                self.next_char();
                r.push(c);
            }
        }
        Err(LexerError::UnexpectedEndOfFile(self.line_info.clone()))
    }

    /**
     * Read a character from the source code.
     */
    fn read_char(&mut self) -> LexResult {
        let mut r = String::new();
        while let Some(c) = self.peek_char() {
            if c == '\'' {
                self.next_char();
                let r = self.resolve_escape_sequence(r);
                if r.len() != 1 {
                    return Err(LexerError::InvalidChar(r, self.line_info.clone()));
                }
                return Ok(Token::Char(r.chars().next().unwrap()));
            } else {
                self.next_char();
                r.push(c);
            }
        }
        Err(LexerError::UnexpectedEndOfFile(self.line_info.clone()))
    }

    /**
     * Read a number from the source code.
     * Can be an integer or a float (casted at runtime).
     */
    fn read_number(&mut self, c: char) -> LexResult {
        let mut r = String::new();
        r.push(c);
        let mut has_dot = false;
        while let Some(c) = self.peek_char() {
            if c.is_numeric() {
                self.next_char();
                r.push(c);
            } else if c == '.' && !has_dot {
                self.next_char();
                r.push(c);
                has_dot = true;
            } else {
                // We have reached the end of the number
                if has_dot {
                    return Ok(Token::Float(r));
                } else {
                    return Ok(Token::Integer(r));
                }
            }
        }
        Err(LexerError::UnexpectedEndOfFile(self.line_info.clone()))
    }

    fn read_identifier(&mut self, c: char) -> LexResult {
        let mut r = String::new();
        r.push(c);
        while let Some(c) = self.peek_char() {
            if c.is_alphanumeric() {
                self.next_char();
                r.push(c);
            } else {
                // We have reached the end of the identifier
                return Ok(Token::Identifier(r));
            }
        }
        Err(LexerError::UnexpectedEndOfFile(self.line_info.clone()))
    }

}

impl<R: BufRead + Seek> Iterator for Lexer<R> {
    type Item = LexResult;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(Token::EndOfFile) => None, // End of iterator stream
            r => Some(r)
        }
    }
}
