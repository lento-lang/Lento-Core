use std::{path::Path, io::{BufReader, Error, BufRead, Seek}, fs::File};

use chumsky::primitive::Container;

use crate::{lexer::{readers::bytes_reader::BytesReader, token::{Token, TokenInfo}, op::{OperatorPrecedence, OperatorPosition, OperatorAssociativity, Operator}, error::LexerError, lexer::LexResult}, interpreter::value::{Value, Number}, type_checker::types::Type};

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

    pub fn reset(&mut self) {
        self.lexer.reset();
    }

    pub fn parse(&mut self) -> Result<Ast, ParseFail> {
        // Ok(unit())
        // Ok(Ast::FunctionCall("print".to_string(), vec![Ast::Literal(Value::String("Hello, world!".to_string()))], None))
        let expr = self.parse_top_expr();
        if let Ok(e) = expr.as_ref() { println!("Parsed expression: {:?}", e); }
        expr
    }

    fn parse_literal(&mut self, token: Token) -> Option<Value> {
        Some(match token {
            Token::Integer(n) => Value::Number(Number::parse(n)),
            Token::Float(n) => Value::Number(Number::parse(n)),
            Token::String(s) => Value::String(s.clone()),
            Token::Char(c) => Value::Char(c),
            Token::Boolean(b) => Value::Boolean(b),
            _ => return None
        })
    }

    fn parse_call(&mut self, id: String) -> Result<Ast, ParseFail> {
        let mut args = Vec::new();
        let nt = self.lexer.next_token();
        if nt.is_err() { return Err(ParseFail { msg: "Expected '('".to_string() }); }
        assert!(nt.unwrap().token == Token::LeftParen);
        while let Ok(t) = self.lexer.peek_token() {
            if t.token == Token::RightParen { break; }
            let expr = self.parse_top_expr()?;
            args.push(expr);
            let nt = self.lexer.peek_token();
            if nt.is_err() { return Err(ParseFail { msg: "Expected ')'".to_string() }); }
            let nt = nt.unwrap().token;
            if nt == Token::RightParen {
                self.lexer.next_token();
                break;
            }
            let nt = self.lexer.next_token();
            if nt.is_err() { return Err(ParseFail { msg: "Expected ')'".to_string() }); }
            let nt = nt.unwrap().token;
            if nt.is_terminator() {
                continue;
            }
            return Err(ParseFail { msg: "Expected ')'".to_string() });
        }
        // TODO: Extract read_until_terminator() helper method
        Ok(Ast::FunctionCall(id, args, None))
    }

    fn parse_primary(&mut self) -> Result<Ast, ParseFail> {
        let nt = self.lexer.next_token();
        if let Ok(t) = nt {
            if t.token.is_literal() {
                Ok(Ast::Literal(self.parse_literal(t.token).unwrap()))
            } else if let Token::Identifier(id) = t.token {
                // Check if function call
                if let Ok(t) = self.lexer.peek_token() {
                    if t.token == Token::LeftParen {
                        return self.parse_call(id);
                    }
                }
                Ok(Ast::Identifier(id, None))
            } else if let Token::TypeIdentifier(t) = t.token {
                Ok(Ast::TypeIdentifier(t, None))
            } else if t.token.is_operator() {
                let op = t.token.get_operator().unwrap();
                if op.pos() != OperatorPosition::Prefix {
                    return Err(ParseFail { msg: format!("Expected prefix operator, found {:?}", op) });
                }
                let rhs = self.parse_primary()?;
                Ok(Ast::Unary(op, Box::new(rhs), None))
            } else {
                Err(ParseFail { msg: format!("Expected primary expression, found {:?}", t) })
            }
        } else {
            Err(ParseFail { msg: format!("Expected primary expression, but failed due to: {:?}", nt.unwrap_err()) })
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
    fn parse_expr(&mut self, lhs: Ast, min_prec: OperatorPrecedence) -> Result<Ast, ParseFail> {
        let mut nt = self.lexer.peek_token();
        let mut expr = lhs;
        let check_first = |op: &LexResult| -> Option<Operator> {
            /* return true if both:
                * the result is ok
                * 'op' is a binary operator whose precedence is greater than min_prec
            */
            if op.is_err() { return None; }
            if let Some(op) = op.as_ref().unwrap().token.get_operator() {
                if op.pos() == OperatorPosition::Infix && op.precedence() >= min_prec { Some(op) }
                else { None }
            } else { None }
        };
        while let Some(op) = check_first(&nt) {
            self.lexer.next_token().unwrap(); // consume the peeked binary operator token
            let mut rhs = self.parse_primary()?;
            nt = self.lexer.peek_token();
            let check_next = |op: &LexResult| {
                /* return true if either:
                    * 'op' is a binary operator whose precedence is greater than op's
                    * 'op' is a right-associative binary operator whose precedence is equal to op's
                 */
                if op.is_err() { return false; }
                else if let Some(op) = op.as_ref().unwrap().token.get_operator() {
                    op.pos() == OperatorPosition::Infix && ((
                        op.precedence() > min_prec
                    ) || (
                        op.associativity() == OperatorAssociativity::Right &&
                        op.precedence() == min_prec
                    ))
                } else { false }
            };
            while check_next(&nt) {
                let op_prec = nt.unwrap().token.get_operator().unwrap().precedence();
                let next_prec = op_prec + (op_prec > min_prec) as OperatorPrecedence;
                rhs = self.parse_expr(rhs, next_prec)?;
                nt = self.lexer.peek_token();
            }
            expr = Ast::Binary(Box::new(expr), op, Box::new(rhs), None);
        }
        Ok(expr)
    }

    /**
     * Parse a top-level expression
     */
    fn parse_top_expr(&mut self) -> Result<Ast, ParseFail> {
        let lhs = self.parse_primary()?;
        self.parse_expr(lhs, 0)
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
