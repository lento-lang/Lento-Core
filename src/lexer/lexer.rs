use std::{
    cell::Cell,
    collections::{HashMap, HashSet},
    fmt::Display,
    fs::File,
    io::{BufReader, Cursor, Error, Read},
    path::PathBuf,
};

use lazy_regex::regex_replace_all;

use crate::{
    interpreter::error::{runtime_error, RuntimeError},
    util::failable::Failable,
};

use super::{
    error::LexerError,
    op::Operator,
    readers::bytes_reader::BytesReader,
    token::{LineInfoSpan, TokenInfo, TokenKind},
};

/// The source type of the program input. \
/// This is used to determine how to read the
/// input for the program.
/// And improve error messages.
#[derive(Debug, Clone, PartialEq)]
pub enum InputSource {
    /// A file path to the program source. \
    /// 1. The string is the path to the file.
    File(PathBuf),
    /// A static string containing the program source.
    String,
    /// A stream of characters. \
    /// 1. The string is the name of the stream.
    Stream(String),
}

impl Display for InputSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InputSource::File(path) => write!(f, "file '{}'", path.to_string_lossy()),
            InputSource::String => write!(f, "string"),
            InputSource::Stream(name) => write!(f, "stream '{}'", name),
        }
    }
}

//--------------------------------------------------------------------------------------//
//                                        Lexer                                         //
//--------------------------------------------------------------------------------------//

pub type LexResult = Result<TokenInfo, LexerError>;

/// Helper function to check if a token matches a predicate.
fn check_token(token: &LexResult, predicate: &impl Fn(&TokenKind) -> bool) -> bool {
    match token {
        Ok(TokenInfo { token, .. }) => predicate(token),
        _ => false,
    }
}

/// The lexer tokenize a program input source and **output a stream of tokens**. \
/// The lexer is a state machine that is used by the parser to generate an AST.
#[derive(Clone)]
pub struct Lexer<R>
where
    R: Read,
{
    input_source: InputSource,
    reader: R,
    /// Everything successfully read from the source code.
    content: Vec<u8>,
    index: usize,
    line_info: LineInfoSpan,
    operators: HashMap<String, Operator>,
    types: HashSet<String>,
    peeked_tokens: Vec<LexResult>, // Queue of peeked tokens (FIFO)
}

impl<R: Read> Lexer<R> {
    /// Create a new lexer from a reader.
    /// The lexer will read from the reader until it reaches the end of the file.
    pub fn new(reader: R, input_source: InputSource) -> Self {
        Self {
            input_source,
            reader,
            content: Vec::new(),
            index: 0,
            line_info: LineInfoSpan::new(),
            operators: HashMap::new(),
            types: HashSet::new(),
            peeked_tokens: Vec::new(),
        }
    }

    /// Create a new lexer from a reader.
    /// The lexer will read from the reader until it reaches the end of the stream, then the parser must call `refill_on_read` to refill the buffer.
    /// If the reader is a stream, the lexer will be able to refill the buffer with new data from the reader on the next read (at any time)
    pub fn new_stream(reader: R, stream_name: String) -> Self {
        Self {
            input_source: InputSource::Stream(stream_name),
            reader,
            content: Vec::new(),
            index: 0,
            line_info: LineInfoSpan::new(),
            operators: HashMap::new(),
            types: HashSet::new(),
            peeked_tokens: Vec::new(),
        }
    }

    pub fn get_reader(&mut self) -> &mut R {
        &mut self.reader
    }

    pub fn current_index(&self) -> usize {
        self.index
    }

    pub fn reset(&mut self) {
        self.content.clear();
        self.index = 0;
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

    pub fn lookup_op(&self, symbol: &str) -> Option<&Operator> {
        self.operators.get(symbol)
    }

    pub fn define_type(&mut self, type_name: String) -> Failable<RuntimeError> {
        if self.types.contains(&type_name) {
            return Err(runtime_error(format!(
                "Cannot override type '{}'",
                type_name
            )));
        }
        self.types.insert(type_name);
        Ok(())
    }

    pub fn is_type(&self, type_name: &str) -> bool {
        self.types.contains(type_name)
    }

    fn new_token_info(&self, token: TokenKind) -> LexResult {
        Ok(TokenInfo {
            info: self.line_info.clone(),
            token,
        })
    }

    /// Set the start of the token to the current position of the lexer.
    /// This function is called when the lexer is about to start reading a new token.
    fn set_info_start_to_current(&mut self) {
        self.line_info.start = self.line_info.end.clone();
    }

    pub fn try_read_chunk(&mut self) -> Option<()> {
        const BUFFER_SIZE: usize = 128;
        let mut buffer: [u8; BUFFER_SIZE] = [0; BUFFER_SIZE];
        match self.reader.read(&mut buffer) {
            Ok(n) => {
                if n == 0 {
                    None
                } else {
                    self.content.extend_from_slice(&buffer[..n]);
                    Some(()) // Success
                }
            }
            Err(_) => None
        }
    }

    fn peek_char(&mut self, offset: usize) -> Option<char> {
        let peek_index = self.index.checked_add(offset)?;
        if peek_index >= self.content.len() {
            self.try_read_chunk()?; // If fail, return None
        }
        let c = self.content[peek_index];
        if c == 0 {
            None
        } else {
            Some(c as char)
        }
    }

    /// Read a string from the source code using a buffered reader.
    fn next_char(&mut self) -> Option<char> {
        if self.index >= self.content.len() {
            self.try_read_chunk()?; // If fail, return None
        }
        let c = self.content[self.index];
        if c == 0 {
            return None;
        }
        self.index += 1;
        self.line_info.end.index += 1;
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

    /// Peek the next token from the source code.
    /// ! Note: This function should not be called from read_next_token() because it will cause an infinite loop (stack overflow)
    pub fn peek_token(&mut self, offset: usize) -> LexResult {
        if let Some(t) = self.get_peeked_token(offset) {
            t
        } else {
            let token = self.next_token();
            // Do not push EOF to the peeked tokens list
            if let Ok(TokenInfo {
                token: TokenKind::EndOfFile,
                ..
            }) = token
            {
                return token;
            }
            self.peeked_tokens.push(token.to_owned());
            self.peek_token(offset)
        }
    }

    /// Get the next token from the source code,
    /// Or return the next peeked token.
    pub fn read_next_token(&mut self) -> LexResult {
        if let Some(token) = self.consume_peeked_token(0) {
            token
        } else {
            self.next_token()
        }
    }

    /// Expect there to be a token (not EOF).
    /// If there is a token, return it.
    /// Else, try to read from the source if the reader is a stream.
    /// Then return the first read token (can be EOF but will throw error later)
    fn expect_next_token(&mut self) -> LexResult {
        let token = self.read_next_token()?;
        if token.token == TokenKind::EndOfFile {
            self.read_next_token()
        } else {
            Ok(token)
        }
    }

    /// Peek the next token from the source code, ignoring tokens that match the predicate.
    /// Or return the next peeked token.
    pub fn peek_token_not(&mut self, predicate: impl Fn(&TokenKind) -> bool) -> LexResult {
        let mut idx = 0usize;
        let mut token = self.peek_token(idx);
        // while Self::check_token(&token, predicate.clone()) {
        //     token = self.peek_token(idx + 1);
        //     idx += 1;
        // }
        while check_token(&token, &predicate) {
            token = self.peek_token(idx + 1);
            idx += 1;
        }
        token
    }

    /// Get the next token from the source code, ignoring tokens that match the predicate.
    /// Or return the next peeked token.
    pub fn read_next_token_not(&mut self, predicate: impl Fn(&TokenKind) -> bool) -> LexResult {
        let mut token = self.read_next_token();
        while check_token(&token, &predicate) {
            token = self.read_next_token();
        }
        token
    }

    /// Expect there to be a token (not EOF) while ignoring tokens that match the predicate.
    /// If there is a token, return it.
    /// Else, try to read from the source if the reader is a stream.
    /// Then return the first read token (can be EOF but will throw error later)
    /// Or return the next peeked token (again, ignoring newlines)
    /// Direct copy of `read_next_token_no_nl` but expecting tokens from the stream.
    pub fn expect_next_token_not(&mut self, predicate: impl Fn(&TokenKind) -> bool) -> LexResult {
        let mut token = self.expect_next_token();
        while check_token(&token, &predicate) {
            token = self.expect_next_token();
        }
        token
    }

    /// Get the next token from the source code.
    /// This function is the main function of the lexer.
    /// This function can only move forward, it cannot go back.
    fn next_token(&mut self) -> LexResult {
        self.set_info_start_to_current();
        if let Some(c) = self.next_char() {
            if c == ' ' || c == '\t' || c == '\r' {
                self.next_token() // Ignore whitespace
            } else if c == '\n' {
                self.line_info.end.line += 1;
                self.line_info.end.column = 1;
                self.new_token_info(TokenKind::Newline)
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
                    '(' => TokenKind::LeftParen,
                    ')' => TokenKind::RightParen,
                    '{' => TokenKind::LeftBrace,
                    '}' => TokenKind::RightBrace,
                    '[' => TokenKind::LeftBracket,
                    ']' => TokenKind::RightBracket,
                    ';' => TokenKind::SemiColon,
                    '/' if self.peek_char(0) == Some('/') => return self.read_comment(),
                    _ => return self.read_operator(c),
                })
            }
        } else {
            self.new_token_info(TokenKind::EndOfFile)
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
        let s = regex_replace_all!(r#"\\n"#, &s, |_| "\n".to_string());
        let s = regex_replace_all!(r#"\\r"#, &s, |_| "\r".to_string());
        let s = regex_replace_all!(r#"\\t"#, &s, |_| "\t".to_string());
        let s = regex_replace_all!(r#"\\0"#, &s, |_| "\0".to_string());
        // let s = regex_replace_all!(r#"\\\""#, &s, |_| "\"".to_string());
        let s = regex_replace_all!(r#"\\'"#, &s, |_| "'".to_string());
        let s = regex_replace_all!(r#"\\\\"#, &s, |_| "\\".to_string());
        s.to_string()
    }

    /// Helper function to read a token from the source code using a predicates and lambdas or function composition.
    fn read_while(
        &mut self,
        init: Option<String>,
        mut cond: impl FnMut(char) -> bool,
        allow_eof_before_cond_false: bool,
        mut build_token: impl FnMut(&mut Self, String) -> LexResult,
        mut post: impl FnMut(&mut Self, &LexResult),
    ) -> LexResult {
        let mut result = init.unwrap_or_default();
        let mut done = |this: &mut Self, result: String| {
            let token = build_token(this, result);
            post(this, &token);
            token
        };
        while let Some(c) = self.peek_char(0) {
            if cond(c) {
                self.next_char();
                result.push(c);
            } else {
                return done(self, result);
            }
        }
        if allow_eof_before_cond_false {
            done(self, result)
        } else {
            Err(LexerError::unexpected_end_of_file(
                self.line_info.clone(),
                self.input_source.clone(),
            ))
        }
    }

    fn read_quoted(
        &mut self,
        quote: char,
        mut build_token: impl FnMut(&mut Self, String) -> LexResult,
    ) -> LexResult {
        let escape = Cell::new(false);
        self.read_while(
            None,
            move |c| {
                if escape.get() {
                    escape.set(false);
                    true
                } else {
                    if c == '\\' {
                        escape.set(true);
                    }
                    c != quote
                }
            },
            false,
            |this, s| build_token(this, Lexer::<R>::resolve_escape_sequence(s)),
            |this, _| {
                this.next_char();
            }, // Eat the last quote
        )
    }

    /// Read a string from the source code.
    fn read_string(&mut self) -> LexResult {
        self.read_quoted('"', |this, s| this.new_token_info(TokenKind::String(s)))
    }

    /// Read a character from the source code.
    fn read_char(&mut self) -> LexResult {
        self.read_quoted('\'', |this, s| {
            if s.len() != 1 {
                Err(LexerError::invalid_char(
                    s,
                    this.line_info.clone(),
                    this.input_source.clone(),
                ))
            } else {
                this.new_token_info(TokenKind::Char(s.chars().next().unwrap()))
            }
        })
    }

    /// Read a number from the source code.
    /// Can be an integer or a float (casted at runtime).
    fn read_number(&mut self, c: char) -> LexResult {
        let has_dot = Cell::new(false);
        self.read_while(
            Some(c.to_string()),
            |c| {
                if c == '.' && !has_dot.get() {
                    has_dot.set(true);
                    true
                } else {
                    c.is_numeric()
                }
            },
            true,
            |this, s| {
                this.new_token_info(if has_dot.get() {
                    TokenKind::Float(s)
                } else {
                    TokenKind::Integer(s)
                })
            },
            |_, _| (),
        )
    }

    fn is_identifier_head_char(c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }
    fn is_identifier_body_char(c: char) -> bool {
        c.is_alphanumeric() || c == '_'
    }

    fn create_identifier_type_or_keyword(&self, s: String) -> TokenKind {
        match s.as_str() {
            "true" => TokenKind::Boolean(true),
            "false" => TokenKind::Boolean(false),
            "let" => TokenKind::Let,
            t if self.is_type(t) => TokenKind::TypeIdentifier(s),
            sym => match self.lookup_op(sym) {
                Some(op) => TokenKind::Op(op.clone()),
                None => TokenKind::Identifier(s),
            }
        }
    }

    /// Read an identifier from the source code.
    fn read_identifier(&mut self, c: char) -> LexResult {
        self.read_while(
            Some(c.to_string()),
            Self::is_identifier_body_char,
            true,
            |this, s| this.new_token_info(this.create_identifier_type_or_keyword(s)),
            |_, _| (),
        )
    }

    /// Read a comment from the source code.
    fn read_comment(&mut self) -> LexResult {
        self.next_char(); // Eat the first '/'
        self.read_while(
            None,
            |c| c != '\n',
            true,
            |this, s| this.new_token_info(TokenKind::Comment(s)),
            |_, _| (),
        )
    }

    fn read_operator(&mut self, first: char) -> LexResult {
        // get all self.operators starting with first
        let mut ops = self
            .operators
            .clone()
            .into_iter()
            .filter(|(k, _)| k.starts_with(first))
            .collect::<HashMap<_, _>>();
         if ops.is_empty() {
            return Err(LexerError::unexpected_character(
                first,
                self.line_info.clone(),
                self.input_source.clone(),
            ));
        }
        let mut longest_match = first.to_string();
        loop {
            let c = if let Some(c) = self.peek_char(0) { c }
            else { break; };
            longest_match.push(c);
            let new_ops = ops.into_iter().filter(|(k, _)| k.starts_with(&longest_match)).collect::<HashMap<_, _>>();
            if new_ops.is_empty() {
                longest_match.pop();
                break;
            }
            ops = new_ops;
            self.next_char(); // Eat the peeked character
        }
        let op = self.lookup_op(&longest_match).unwrap(); // Safe because we know the operator exists
        self.new_token_info(TokenKind::Op(op.clone()))
    }
}

pub fn from_str(input: &str) -> Lexer<BytesReader<'_>> {
    Lexer::new(BytesReader::from(input), InputSource::String)
}

pub fn from_string(input: String) -> Lexer<Cursor<String>> {
    Lexer::new(Cursor::new(input), InputSource::String)
}

pub fn from_file(file: File, path: PathBuf) -> Lexer<BufReader<File>> {
    Lexer::new(BufReader::new(file), InputSource::File(path))
}

pub fn from_path(path: PathBuf) -> Result<Lexer<BufReader<File>>, Error> {
    Ok(Lexer::new(
        BufReader::new(File::open(path.clone())?),
        InputSource::File(path),
    ))
}

pub fn from_stream<R: Read>(reader: R, name: String) -> Lexer<R> {
    Lexer::new_stream(reader, name)
}
