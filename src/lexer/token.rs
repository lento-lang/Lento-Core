// Token structure for the Lento programming language
#[derive(Clone)]
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
    // Built-in operators, these are not overloadable and are reserved for the language
    // All other operators will be implemented in a standard library at runtime in the future
    // leaving support for user-defined operators
    OpAssign,
}

#[derive(Clone)]
pub struct LineInfo {
    pub index: usize,
    pub line: usize,
    pub column: usize,
}
#[derive(Clone)]
pub struct LineInfoSpan {
    pub start: LineInfo,
    pub end: LineInfo,
}

impl LineInfoSpan {
    pub fn new() -> Self {
        let empty = LineInfo {
            index: 0,
            line: 0,
            column: 0,
        };
        Self {
            start: empty.clone(),
            end: empty,
        }
    }
}

#[derive(Clone)]
pub struct TokenInfo {
    pub token: Token,
    pub info: LineInfoSpan,
}
