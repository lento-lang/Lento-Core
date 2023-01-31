

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String
}


impl ParseError {
    pub fn new(message: String) -> Self {
        Self {
            message
        }
    }
}
