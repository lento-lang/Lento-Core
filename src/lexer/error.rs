use super::token::LineInfoSpan;

#[derive(Debug, Clone)]
pub struct LexerError {
    pub message: String,
    pub info: LineInfoSpan,
}

impl LexerError {
    pub fn new(message: String, info: LineInfoSpan) -> Self {
        Self { message, info }
    }

    pub fn unexpected_end_of_file(info: LineInfoSpan) -> Self {
        Self::new("Unexpected end of file".to_string(), info)
    }

    pub fn unexpected_character(c: char, info: LineInfoSpan) -> Self {
        Self::new(format!("Unexpected character '{}'", c), info)
    }

    pub fn invalid_char(c: String, info: LineInfoSpan) -> Self {
        Self::new(format!("Invalid character literal '{}'", c), info)
    }
}
