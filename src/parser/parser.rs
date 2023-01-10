use std::{path::Path, io::{BufReader, Error, BufRead, Seek}, fs::File};

use crate::lexer::readers::string_reader::BytesReader;

use crate::lexer::lexer::Lexer;

use super::ast::{Ast, AstNode};

#[derive(Debug, Clone, PartialEq)]
pub enum ParserInput<'a> {
    File(&'a Path),
    Other(&'a str)
}

#[derive(Debug, Clone)]
pub struct  ParseFail {
    pub msg: String
}

// A stream-lined parser for Lento with support for user-defined operators from function attributes and macros
#[derive(Clone)]
pub struct Parser<'a, R> {
    lexer: Lexer<R>,
    input: ParserInput<'a>
}

impl<'a, R: BufRead + Seek> Parser<'a, R> {
    pub fn new(lexer: Lexer<R>, input: ParserInput<'a>) -> Self {
        Self {
            lexer,
            input
        }
    }

    pub fn parse(&mut self) -> Result<Ast, ParseFail> {
        Ok(Ast { name: "root".to_string(), value: Box::new(AstNode::Unit) })
    }
}

pub fn from_file<'a>(file: File, path: &'a Path) -> Parser<'a, BufReader<File>> {
    Parser::new(
        Lexer::new(BufReader::new(file)), 
        ParserInput::File(path)
    )
}

pub fn from_path<'a>(source_file: &'a Path) -> Result<Parser<'a, BufReader<File>>, Error> {
    Ok(from_file(File::open(source_file)?, source_file))
}

pub fn from_string<'a>(source: &'a String, title: &'a str) -> Parser<'a, BufReader<BytesReader<'a>>> {
    Parser::new(
        Lexer::new(BufReader::new(BytesReader::from(source))), 
        ParserInput::Other(title)
    )
}

pub fn from_str<'a>(source: &'a str, title: &'a str) -> Parser<'a, BufReader<BytesReader<'a>>> {
    Parser::new(
        Lexer::new(BufReader::new(BytesReader::from(source))), 
        ParserInput::Other(title)
    )
}

pub fn parse_string(source: String, title: &str) -> Result<Ast, ParseFail> {
    from_string(&source, title)
        .parse()
}

pub fn parse_str(source: &str, title: &str) -> Result<Ast, ParseFail> {
    from_str(source, title)
        .parse()
}

pub fn parse_file(file: File, path: &Path) -> Result<Ast, ParseFail> {
    from_file(file, path)
        .parse()
}

pub fn parse_from_path(path: &Path) -> Result<Result<Ast, ParseFail>, Error> {
    Ok(from_path(path)?
        .parse())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser_hello_world() {
        let code = "print(\"Hello, World!\")";
        let result = parse_str(code, "test.lento.print.hello_world");
        assert!(result.is_ok());
        assert!(result.unwrap().value.as_ref() == &AstNode::Unit);
    }

    #[test]
    fn test_parser_hello_world_file() {
        let result = parse_from_path(Path::new("./examples/basic/hello_world.lt"));
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.is_ok());
        assert!(result.unwrap().value.as_ref() == &AstNode::Unit);
    }
}
