use super::token::LineInfoSpan;

// Lexer error
pub struct LexerError {
    pub message: String,
    pub info: LineInfoSpan,
}

impl LexerError {
    pub fn new(message: String, info: LineInfoSpan) -> Self {
        Self {
            message,
            info
        }
    }

    pub fn UnexpectedEndOfFile(info: LineInfoSpan) -> Self {
        Self::new("Unexpected end of file".to_string(), info)
    }

    pub fn UnexpectedCharacter(c: char, info: LineInfoSpan) -> Self {
        Self::new(format!("Unexpected character '{}'", c), info)
    }

    pub fn InvalidChar(c: String, info: LineInfoSpan) -> Self {
        Self::new(format!("Invalid character literal '{}'", c), info)
    }
}
