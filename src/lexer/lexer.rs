use std::{io::{Seek, BufRead}, collections::HashMap};

use lazy_regex::{self, regex_replace_all};

use super::{token::{Token, LineInfoSpan}, error::LexerError, op::Operator};

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
    first_read: bool,
    buffer_idx: usize,
    line_info: LineInfoSpan,
    operators: HashMap<String, Operator>
}

impl<R: BufRead + Seek> Lexer<R> {
    pub fn new(reader: R) -> Self {
        Self {
            reader,
            buffer: [0; BUFFER_SIZE],
            first_read: true,
            buffer_idx: 0,
            line_info: LineInfoSpan::new(),
            operators: HashMap::new()
        }
    }

    pub fn define_op(&mut self, op: Operator) {
        self.operators.insert(op.symbol().clone(), op);
    }

    /**
     * Set the start of the token to the current position of the lexer.
     * This function is called when the lexer is about to start reading a new token.
     */
    fn set_info_start_to_current(&mut self) {
        self.line_info.start = self.line_info.end.clone();
    }

    fn peek_char(&mut self, offset: i64) -> Option<char> {
        let prev_idx = self.reader.seek(std::io::SeekFrom::Current(0)).unwrap();
        self.reader.seek(std::io::SeekFrom::Current(offset)).unwrap();
        let c = &mut [0; 1];
        let reset = |reader: &mut R| { reader.seek(std::io::SeekFrom::Start(prev_idx)).unwrap() }; // Reset the position of the reader
        if let Ok(n) = self.reader.read(c) {
            reset(&mut self.reader);
            if n > 0 { Some(c[0] as char) }
            else { None }
        } else {
            reset(&mut self.reader);
            None
        }
    }

    /**
     * Read a string from the source code using a buffered reader.
     */
    fn next_char(&mut self) -> Option<char> {
        if self.buffer_idx >= BUFFER_SIZE || self.first_read {
            if self.reader.read(&mut self.buffer).is_err() { return None; }
            self.first_read = false;
            self.buffer_idx = 0;
        }
        let c = self.buffer[self.buffer_idx];
        if c == 0 { return None; }
        self.buffer_idx += 1;
        self.line_info.end.column += 1;
        Some(c as char)
    }

    fn lookup_operator(&self, op: &str) -> Option<Operator> {
        // TODO: Incremental search using peek_char and break when there is no match,
        //      otherwise, return the longest match while peeking all the way to the end and "eating" the characters if there is a match.
        self.operators.get(op).cloned()
    }

    /**
     * Get the next token from the source code, ignoring newlines.
     */
    pub fn next_token_no_nl(&mut self) -> LexResult {
        let mut token = self.next_token()?;
        while token == Token::Newline {
            token = self.next_token()?;
        }
        Ok(token)
    }

    /**
     * Get the next token from the source code.
     * This function is the main function of the lexer.
     */
    pub fn next_token(&mut self) -> LexResult {
        if let Some(c) = self.next_char() {
            if c == ' ' || c == '\t' || c == '\r' {
                self.next_token() // Ignore whitespace
            } else if c == '\n' {
                self.line_info.end.line += 1;
                self.line_info.end.column = 0;
                Ok(Token::Newline)
            } else if c == '"' {
                self.read_string()
            } else if c == '\'' {
                self.read_char()
            } else if c.is_numeric() {
                self.read_number(c)
            } else if Self::is_identifier_head_char(c) {
                self.read_identifier(c)
            } else { Ok(match c {
                '(' => Token::LeftParen,
                ')' => Token::RightParen,
                '{' => Token::LeftBrace,
                '}' => Token::RightBrace,
                '[' => Token::LeftBracket,
                ']' => Token::RightBracket,
                _ => {
                    if let Some(op) = self.lookup_operator(&c.to_string()) {
                        Token::Op(op)
                    } else {
                        return Err(LexerError::unexpected_character(c, self.line_info.clone()));
                    }
                }
            }) }
        } else {
            Ok(Token::EndOfFile)
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
    fn read_while(&mut self, init: Option<String>, mut cond: impl FnMut(char) -> bool, mut build_token: impl FnMut(&mut Self, String) -> LexResult) -> LexResult {
        let mut r = init.unwrap_or(String::new());
        while let Some(c) = self.peek_char(0) {
            println!("read_while: checking cond('{}')", c);
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
            Ok(Token::String(Lexer::<R>::resolve_escape_sequence(s)))
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
                Ok(Token::Char(s.chars().next().unwrap()))
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
        }, |_, s| Ok(if has_dot { Token::Float(s) } else { Token::Integer(s) }))
    }

    fn is_identifier_head_char(c: char) -> bool { c.is_alphabetic() || c == '_' }
    fn is_identifier_body_char(c: char) -> bool { c.is_alphanumeric() || c == '_' }

    /**
     * Read an identifier from the source code.
     */
    fn read_identifier(&mut self, c: char) -> LexResult {
        self.read_while(Some(c.to_string()), Self::is_identifier_body_char, |_, s| Ok(Token::Identifier(s)))
    }

}
