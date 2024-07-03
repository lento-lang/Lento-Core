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

/// Errors for when an operator is inserted into the parser
/// operator table.
#[derive(Debug, PartialEq)]
pub enum OperatorError {
    SignatureForSymbolExists,
    SymbolNotOverloadable,
    PositionForSymbolExists,
}

/// Errors for when a type is inserted into the parser
/// type table.
#[derive(Debug, PartialEq)]
pub enum TypeError {
    TypeExists,
    NonLiteralType,
}
