// Token structure for the Lento programming language
#[derive(Clone)]
pub enum Token {
    EndOfFile,
    // Expression terminators
    Newline,
    SemiColon,
    // Literals
    Identifier(String),
    Integer(i64),
    Float(f64),
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
