#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub message: String,
    pub span: (usize, usize),
}

impl ParseError {
    pub fn new(message: String, span: (usize, usize)) -> Self {
        Self { message, span }
    }
}
