use std::fmt::{Debug, Display};

use crate::interpreter::number::Number;

// Token structure for the Lento programming language
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    EndOfFile,
    // Expression terminators
    Newline,
    SemiColon,
    Colon,
    Comma,
    // Literals
    Identifier(String),
    Number(Number),
    String(String),
    Char(char),
    Boolean(bool),
    TypeIdentifier(String),
    // Grouping and separation tokens
    LeftParen,    // (
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    LeftBracket,  // [
    RightBracket, // ]
    // All other operators will be implemented in a standard library at runtime in the future
    // leaving support for user-defined operators
    Op(String),
    // Keywords
    Let,
    // Comments
    Comment(String),
}

impl TokenKind {
    pub fn is_literal(&self) -> bool {
        matches!(
            self,
            TokenKind::Number(_)
                | TokenKind::String(_)
                | TokenKind::Char(_)
                | TokenKind::Boolean(_)
        )
    }

    pub fn is_terminator(&self) -> bool {
        matches!(
            self,
            TokenKind::EndOfFile
                | TokenKind::Newline
                | TokenKind::SemiColon
                | TokenKind::RightParen
                | TokenKind::RightBrace
                | TokenKind::RightBracket
                | TokenKind::Comment(_)
        )
    }

    pub fn is_grouping_start(&self) -> bool {
        matches!(
            self,
            TokenKind::LeftParen | TokenKind::LeftBrace | TokenKind::LeftBracket
        )
    }

    pub fn is_grouping_end(&self) -> bool {
        matches!(
            self,
            TokenKind::RightParen | TokenKind::RightBrace | TokenKind::RightBracket
        )
    }

    pub fn is_grouping(&self) -> bool {
        self.is_grouping_start() || self.is_grouping_end()
    }

    pub fn is_top_level_terminal(&self, allow_eof: bool) -> bool {
        if allow_eof {
            matches!(
                self,
                TokenKind::EndOfFile | TokenKind::Newline | TokenKind::SemiColon
            )
        } else {
            matches!(self, TokenKind::Newline | TokenKind::SemiColon)
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EndOfFile => write!(f, "EndOfFile"),
            Self::Newline => write!(f, "newline"),
            Self::SemiColon => write!(f, ";"),
            Self::Colon => write!(f, ":"),
            Self::Comma => write!(f, ","),
            Self::Identifier(s) => write!(f, "{}", s),
            Self::Number(s) => write!(f, "{}", s),
            Self::String(s) => write!(f, "\"{}\"", s),
            Self::Char(c) => write!(f, "'{}'", c),
            Self::Boolean(b) => write!(f, "{}", b),
            Self::TypeIdentifier(s) => write!(f, "{}", s),
            Self::LeftParen => write!(f, "("),
            Self::RightParen => write!(f, ")"),
            Self::LeftBrace => write!(f, "{{"),
            Self::RightBrace => write!(f, "}}"),
            Self::LeftBracket => write!(f, "["),
            Self::RightBracket => write!(f, "]"),
            Self::Op(s) => write!(f, "{}", s),
            Self::Let => write!(f, "let"),
            Self::Comment(s) => write!(f, "// {}", s),
        }
    }
}

#[derive(Clone)]
pub struct LineInfo {
    pub index: usize,
    pub line: usize,
    pub column: usize,
}

impl Debug for LineInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "index: {}, line: {}, column: {}",
            self.index, self.line, self.column
        )
    }
}

#[derive(Clone)]
pub struct LineInfoSpan {
    pub start: LineInfo,
    pub end: LineInfo,
}

impl Debug for LineInfoSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?}) to ({:?})", self.start, self.end)
    }
}

impl Default for LineInfoSpan {
    fn default() -> Self {
        Self::new()
    }
}

impl LineInfoSpan {
    pub fn new() -> Self {
        let empty = LineInfo {
            index: 0,
            line: 1,
            column: 1,
        };
        Self {
            start: empty.clone(),
            end: empty,
        }
    }
}

/// TokenInfo is a structure that contains a token and its line and column information
/// along with the character before and after the token.
/// This is used for error reporting and debugging.
#[derive(Clone)]
pub struct TokenInfo {
    /// The token itself
    pub token: TokenKind,
    /// The line and column of the token
    pub info: LineInfoSpan,
}

impl Debug for TokenInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} at {:?}", self.token, self.info)
    }
}
