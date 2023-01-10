// Lexer error
pub struct LexerError {
    pub message: String,
    pub index_start: usize,
    pub index_end: usize,
    pub line_start: usize,
    pub line_end: usize,
    pub column_start: usize,
    pub column_end: usize,
}
