use std::path::Path;

use super::ast::{Ast, AstNode};



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