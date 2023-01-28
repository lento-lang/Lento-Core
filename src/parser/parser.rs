use std::{path::Path, io::{BufReader, Error, BufRead, Seek}, fs::File};

use crate::lexer::{readers::bytes_reader::BytesReader, token::{Token, TokenInfo}};

use crate::lexer::lexer::Lexer;

use super::ast::{Ast, unit};

#[derive(Debug, Clone)]
pub struct  ParseFail {
    pub msg: String
}

// A stream-lined parser for Lento with support for user-defined operators from function attributes and macros
#[derive(Clone)]
pub struct Parser<R: BufRead + Seek> {
    lexer: Lexer<R>
}

impl<R: BufRead + Seek> Parser<R> {
    pub fn new(lexer: Lexer<R>) -> Self {
        Self {
            lexer
        }
    }

    pub fn parse(&mut self) -> Result<Ast, ParseFail> {
        // Read all tokens from the lexer first to test the lexer
        let mut toks: Vec<TokenInfo> = vec![];
        loop {
            match self.lexer.next_token_no_nl() {
                Ok(token) => {
                    if token.token == Token::EndOfFile { break; }
                    toks.push(token);
                },
                Err(err) => {
                    return Err(ParseFail {
                        msg: err.message
                    })
                }
            }
        }

        println!("{:?}", toks);
        assert!(toks.len() >= 4, "Expected there to be exactly four tokens");
        // Ok(unit())
        Ok(Ast::FunctionCall("print".to_string(), vec![Ast::String("Hello, world!".to_string())], None))
    }
}

//--------------------------------------------------------------------------------------//
//                               Parser Factory Functions                               //
//--------------------------------------------------------------------------------------//

pub fn from_file(file: File) -> Parser<BufReader<File>> {
    Parser::new(
        Lexer::new(BufReader::new(file))
    )
}

pub fn from_path(source_file: &Path) -> Result<Parser<BufReader<File>>, Error> {
    Ok(from_file(File::open(source_file)?))
}

pub fn from_string<'a>(source: &'a String) -> Parser<BufReader<BytesReader<'a>>> {
    Parser::new(
        Lexer::new(BufReader::new(BytesReader::from(source)))
    )
}

pub fn from_str<'a>(source: &'a str) -> Parser<BufReader<BytesReader<'a>>> {
    Parser::new(
        Lexer::new(BufReader::new(BytesReader::from(source)))
    )
}

//--------------------------------------------------------------------------------------//
//                           Direct Parsing Helper Functions                            //
//--------------------------------------------------------------------------------------//

pub fn parse_string(source: String) -> Result<Ast, ParseFail> {
    from_string(&source)
        .parse()
}

pub fn parse_str(source: &str) -> Result<Ast, ParseFail> {
    from_str(source)
        .parse()
}

pub fn parse_file(file: File) -> Result<Ast, ParseFail> {
    from_file(file)
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
        let result = parse_str("print(\"Hello, World!\")");
        assert!(result.is_ok());
        assert!(result.unwrap() == unit());
    }

    #[test]
    fn test_parser_hello_world_file() {
        let result = parse_from_path(Path::new("./examples/basic/hello_world.lt"));
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.is_ok());
        assert!(result.unwrap() == unit());
    }
}
