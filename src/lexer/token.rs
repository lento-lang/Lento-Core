use super::op::Operator;

// Token structure for the Lento programming language
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenKind {
    EndOfFile,
    // Expression terminators
    Newline,
    Comma,
    SemiColon,
    // Literals
    Identifier(String),
    Integer(String),
    Float(String),
    String(String),
    Char(char),
    Boolean(bool),
    TypeIdentifier(String),
    // Grouping and separation tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    // All other operators will be implemented in a standard library at runtime in the future
    // leaving support for user-defined operators
    Op(Operator),
    // Keywords
    Let,
    // Comments
    Comment(String),
}

impl TokenKind {
    pub fn is_operator(&self) -> bool {
        matches!(self, TokenKind::Op(_))
    }

    pub fn get_operator(&self) -> Option<Operator> {
        match self {
            TokenKind::Op(op) => Some(op.clone()),
            _ => None,
        }
    }

    pub fn is_literal(&self) -> bool {
        matches!(
            self,
            TokenKind::Integer(_)
                | TokenKind::Float(_)
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
                | TokenKind::Comma
                | TokenKind::RightParen
                | TokenKind::RightBrace
                | TokenKind::RightBracket
                | TokenKind::Comment(_)
        )
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

#[derive(Debug, Clone)]
pub struct LineInfo {
    pub index: usize,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone)]
pub struct LineInfoSpan {
    pub start: LineInfo,
    pub end: LineInfo,
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
#[derive(Debug, Clone)]
pub struct TokenInfo {
    /// The token itself
    pub token: TokenKind,
    /// The line and column of the token
    pub info: LineInfoSpan,
}
