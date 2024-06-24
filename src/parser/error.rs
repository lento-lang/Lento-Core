use std::fmt::Debug;

#[derive(Clone, PartialEq)]
pub struct ParseError {
    pub message: String,
    pub span: (usize, usize),
}

impl ParseError {
    pub fn new(message: String, span: (usize, usize)) -> Self {
        Self { message, span }
    }
}

impl Debug for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} in {:?}", self.message, self.span)
    }
}
