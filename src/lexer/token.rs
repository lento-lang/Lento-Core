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
    Boolean(bool),
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

impl Token {
    pub fn is_operator(&self) -> bool {
        match self {
            Token::Op(_) => true,
            _ => false,
        }
    }

    pub fn get_operator(&self) -> Option<Operator> {
        match self {
            Token::Op(op) => Some(op.clone()),
            _ => None,
        }
    }

    pub fn is_literal(&self) -> bool {
        match self {
            Token::Integer(_) => true,
            Token::Float(_) => true,
            Token::String(_) => true,
            Token::Char(_) => true,
            Token::Boolean(_) => true,
            _ => false,
        }
    }

    pub fn is_terminator(&self) -> bool {
        match self {
            Token::Newline => true,
            Token::SemiColon => true,
            _ => false,
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
