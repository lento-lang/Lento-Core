use std::{
    fs::File,
    io::{BufReader, Error, Read, Seek},
    path::Path,
};

use crate::{
    interpreter::{
        error::RuntimeError,
        value::{Number, Value},
    },
    lexer::{
        lexer::LexResult,
        op::{Operator, OperatorAssociativity, OperatorPosition, OperatorPrecedence},
        readers::{bytes_reader::BytesReader, stdin::StdinReader},
        token::Token,
    },
    stdlib::init::init_lexer,
    type_checker::types::CheckedType,
};

use crate::lexer::lexer::Lexer;

use super::{
    ast::{unit, Ast},
    error::ParseError,
};

//--------------------------------------------------------------------------------------//
//                                        Parser                                        //
//--------------------------------------------------------------------------------------//

pub type ParseResult = Result<Ast, ParseError>;

// A stream-lined parser for Lento with support for user-defined operators from function attributes and macros
#[derive(Clone)]
pub struct Parser<R>
where
    R: Read + Seek,
{
    lexer: Lexer<R>,
}

impl<R: Read + Seek> Parser<R> {
    pub fn new(lexer: Lexer<R>) -> Self {
        Self { lexer }
    }

    pub fn get_lexer(&mut self) -> &mut Lexer<R> {
        &mut self.lexer
    }

    /**
     * Parse an expression from the stream of tokens.
     * Returns an AST node or an error.
     * If the first token is an EOF, then the parser will return an empty unit expression.
     */
    pub fn parse(&mut self) -> ParseResult {
        if let Ok(t) = self.lexer.peek_token_no_nl() {
            if t.token == Token::EndOfFile {
                return Ok(unit());
            }
        }
        let expr = self.parse_top_expr();
        // if let Ok(e) = expr.as_ref() { println!("Parsed expression: {:?}", e); }
        expr
    }

    fn parse_literal(&mut self, token: Token) -> Option<Value> {
        Some(match token {
            Token::Integer(n) => Value::Number(Number::parse(n)),
            Token::Float(n) => Value::Number(Number::parse(n)),
            Token::String(s) => Value::String(s.clone()),
            Token::Char(c) => Value::Char(c),
            Token::Boolean(b) => Value::Boolean(b),
            _ => return None,
        })
    }

    fn parse_call(&mut self, id: String) -> ParseResult {
        let mut args = Vec::new();
        let nt = self.lexer.read_next_token_no_nl();
        if nt.is_err() {
            return Err(ParseError {
                message: "Expected '('".to_string(),
            });
        }
        assert!(nt.unwrap().token == Token::LeftParen);
        while let Ok(t) = self.lexer.peek_token(0) {
            if t.token == Token::RightParen {
                break;
            }
            let expr = self.parse_top_expr()?;
            args.push(expr);
            let nt = self.lexer.peek_token(0);
            if nt.is_err() {
                return Err(ParseError {
                    message: "Expected ')'".to_string(),
                });
            }
            let nt = nt.unwrap().token;
            if nt == Token::RightParen {
                if let Err(err) = self.lexer.read_next_token() {
                    return Err(ParseError {
                        message: format!("Expected ')' but failed with: {}", err.message),
                    });
                }
                break;
            }
            let nt = self.lexer.read_next_token();
            if nt.is_err() {
                return Err(ParseError {
                    message: "Expected ')'".to_string(),
                });
            }
            let nt = nt.unwrap().token;
            if nt.is_terminator() {
                continue;
            }
            return Err(ParseError {
                message: "Expected ')'".to_string(),
            });
        }
        // TODO: Extract read_until_terminator() helper method
        Ok(Ast::FunctionCall(id, args, CheckedType::Unchecked))
    }

    fn parse_primary(&mut self) -> ParseResult {
        let nt = self.lexer.expect_next_token_no_nl();
        if let Ok(t) = nt {
            if t.token.is_literal() {
                Ok(Ast::Literal(self.parse_literal(t.token).unwrap()))
            } else if let Token::Identifier(id) = t.token {
                // Check if function call
                if let Ok(t) = self.lexer.peek_token(0) {
                    if t.token == Token::LeftParen {
                        return self.parse_call(id);
                    }
                }
                Ok(Ast::Identifier(id, CheckedType::Unchecked))
            } else if let Token::TypeIdentifier(t) = t.token {
                Ok(Ast::TypeIdentifier(t, CheckedType::Unchecked))
            } else if t.token.is_operator() {
                let op = t.token.get_operator().unwrap();
                if op.pos() != OperatorPosition::Prefix {
                    return Err(ParseError {
                        message: format!(
                            "Expected prefix operator, but found {:?} ({})",
                            op.symbol(),
                            op.name()
                        ),
                    });
                }
                let rhs = self.parse_primary()?;
                Ok(Ast::Unary(op, Box::new(rhs), CheckedType::Unchecked))
            } else {
                Err(ParseError {
                    message: format!("Expected primary expression, but found {:?}", t.token),
                })
            }
        } else {
            Err(ParseError {
                message: format!(
                    "Expected primary expression, but failed due to: {:?}",
                    nt.unwrap_err().message
                ),
            })
        }
    }

    /**
     * Parse an expression with a given left-hand side and minimum precedence level
     * using the operator precedence parsing algorithm
     * @param lhs The left-hand side of the expression
     * @param min_prec The minimum precedence of the expression
     * @return The parsed expression
     * @throws ParseFail if the expression could not be parsed
     *
     * @see https://en.wikipedia.org/wiki/Operator-precedence_parser
     * @see https://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing
     * @see https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm
     */
    fn parse_expr(&mut self, lhs: Ast, min_prec: OperatorPrecedence) -> ParseResult {
        let mut nt = self.lexer.peek_token(0);
        // println!("parse_expr: nt = {:?}", nt);
        let mut expr = lhs;
        // println!("parse_expr: expr = {:?}", expr);
        let check_first = |op: &LexResult| -> Option<Operator> {
            /* return true if both:
             * the result is ok
             * 'op' is a binary operator whose precedence is greater than min_prec
             */
            if op.is_err() {
                return None;
            }
            if let Some(op) = op.as_ref().unwrap().token.get_operator() {
                if op.pos() == OperatorPosition::Infix && op.precedence() >= min_prec {
                    Some(op)
                } else {
                    None
                }
            } else {
                None
            }
        };
        while let Some(op) = check_first(&nt) {
            self.lexer.read_next_token().unwrap(); // consume the peeked binary operator token
            let mut rhs = self.parse_primary()?;
            nt = self.lexer.peek_token(0);
            let check_next = |op: &LexResult| {
                /* return true if either:
                 * 'op' is a binary operator whose precedence is greater than op's
                 * 'op' is a right-associative binary operator whose precedence is equal to op's
                 */
                if op.is_err() {
                    return false;
                } else if let Some(op) = op.as_ref().unwrap().token.get_operator() {
                    op.pos() == OperatorPosition::Infix
                        && ((op.precedence() > min_prec)
                            || (op.associativity() == OperatorAssociativity::Right
                                && op.precedence() == min_prec))
                } else {
                    false
                }
            };
            while check_next(&nt) {
                let op_prec = nt.unwrap().token.get_operator().unwrap().precedence();
                let next_prec = op_prec + (op_prec > min_prec) as OperatorPrecedence;
                rhs = self.parse_expr(rhs, next_prec)?;
                nt = self.lexer.peek_token(0);
            }
            expr = Ast::Binary(Box::new(expr), op, Box::new(rhs), CheckedType::Unchecked);
        }
        Ok(expr)
    }

    /**
     * Parse a top-level expression
     */
    fn parse_top_expr(&mut self) -> ParseResult {
        let lhs = self.parse_primary()?;
        self.parse_expr(lhs, 0)
    }
}

//--------------------------------------------------------------------------------------//
//                               Parser Factory Functions                               //
//--------------------------------------------------------------------------------------//

pub fn from_file(file: File) -> Parser<BufReader<File>> {
    let mut lexer = Lexer::new(BufReader::new(file));
    init_lexer(&mut lexer);
    Parser::new(lexer)
}

pub fn from_path(source_file: &Path) -> Result<Parser<BufReader<File>>, Error> {
    Ok(from_file(File::open(source_file)?))
}

pub fn from_string<'a>(source: &'a String) -> Parser<BytesReader<'a>> {
    let mut lexer = Lexer::new(BytesReader::from(source));
    init_lexer(&mut lexer);
    Parser::new(lexer)
}

pub fn from_str<'a>(source: &'a str) -> Parser<BytesReader<'a>> {
    let mut lexer = Lexer::new(BytesReader::from(source));
    init_lexer(&mut lexer);
    Parser::new(lexer)
}

pub fn from_stdin() -> Parser<StdinReader> {
    let mut lexer = Lexer::new_stream(StdinReader::new(std::io::stdin()));
    init_lexer(&mut lexer);
    Parser::new(lexer)
}

//--------------------------------------------------------------------------------------//
//                           Direct Parsing Helper Functions                            //
//--------------------------------------------------------------------------------------//

pub fn parse_string(source: String) -> ParseResult {
    from_string(&source).parse()
}

pub fn parse_str(source: &str) -> ParseResult {
    from_str(source).parse()
}

pub fn parse_file(file: File) -> ParseResult {
    from_file(file).parse()
}

pub fn parse_from_path(path: &Path) -> Result<ParseResult, Error> {
    Ok(from_path(path)?.parse())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser_hello_world() {
        let result = parse_str("println(\"Hello, World!\")");
        let expected = Ast::FunctionCall(
            "println".to_string(),
            vec![Ast::Literal(Value::String("Hello, World!".to_string()))],
            CheckedType::Unchecked,
        );
        assert!(result.is_ok());
        assert!(result.unwrap() == expected);
    }

    #[test]
    fn test_parser_hello_world_file() {
        let result = parse_from_path(Path::new("./examples/basic/hello_world.lt"));
        let expected = Ast::FunctionCall(
            "println".to_string(),
            vec![Ast::Literal(Value::String("Hello, World!".to_string()))],
            CheckedType::Unchecked,
        );
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.is_ok());
        assert!(result.unwrap() == expected);
    }
}
