use super::{lexer::InputSource, token::LineInfoSpan};

#[derive(Debug, Clone)]
pub struct LexerError {
    pub message: String,
    pub info: LineInfoSpan,
    pub input_source: InputSource,
}

impl LexerError {
    pub fn new(message: String, info: LineInfoSpan, input_source: InputSource) -> Self {
        Self {
            message,
            info,
            input_source,
        }
    }

    pub fn unexpected_end_of_file(info: LineInfoSpan, input_source: InputSource) -> Self {
        Self::new(
            format!("Unexpected end of {}", input_source),
            info,
            input_source,
        )
    }

    pub fn unexpected_character(c: char, info: LineInfoSpan, input_source: InputSource) -> Self {
        Self::new(format!("Unexpected character '{}'", c), info, input_source)
    }

    pub fn invalid_char(c: String, info: LineInfoSpan, input_source: InputSource) -> Self {
        Self::new(
            format!("Invalid character literal '{}'", c),
            info,
            input_source,
        )
    }
}
