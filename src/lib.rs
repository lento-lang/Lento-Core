use std::path::Path;

pub mod conf;

#[derive(Debug, Clone)]
pub enum AstNode {
    Root(Ast),
    Print(String),
}
#[derive(Debug, Clone)]
pub struct Ast {
    pub name: String,
    pub value: Box<AstNode>
}

#[derive(Debug, Clone)]
pub struct ParseSuccess<'a> {
    pub source_file: &'a Path,
    pub ast: Ast
}
#[derive(Debug, Clone)]
pub struct  ParseFail<'a> {
    pub source_file: &'a Path,
    pub msg: String
}

pub fn parse_file(file: &Path) -> Result<ParseSuccess, ParseFail> {
    Ok(ParseSuccess {
        source_file: file,
        ast: Ast { name: "root".to_string(), value: Box::new(AstNode::Print("Hello, World!".to_string())) }
    })
}

pub fn interpret_file(file: &Path) -> Result<(), (u8, String)> {
    Ok(())
}

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
