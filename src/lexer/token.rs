use super::op::Operator;

// Token structure for the Lento programming language
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    EndOfFile,
    // Expression terminators
    Newline,
    SemiColon,
    // Literals
    Identifier(String),
    Integer(String),
    Float(String),
    String(String),
    Char(char),
    TypeIdentifier(String),
    // Type expression tokens
    TypeLeftAngleBracket,
    TypeRightAngleBracket,
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

#[derive(Debug, Clone)]
pub struct TokenInfo {
    pub token: Token,
    pub info: LineInfoSpan,
}
