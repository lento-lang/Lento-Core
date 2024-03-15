use std::{
    fs::File,
    io::{BufRead, BufReader, Cursor, Error, Read, Seek},
    path::Path,
};

use crate::{
    interpreter::value::{Number, Value},
    lexer::{
        lexer::{self, InputSource, LexResult},
        op::{Operator, OperatorAssociativity, OperatorPosition, OperatorPrecedence},
        readers::{bytes_reader::BytesReader, stdin::StdinReader},
        token::Token,
    },
    stdlib::init::init_lexer,
    type_checker::types::CheckedType,
};

use crate::lexer::lexer::Lexer;

use super::{
    ast::{unit, Ast, Module},
    error::ParseError,
};

//--------------------------------------------------------------------------------------//
//                                        Parser                                        //
//--------------------------------------------------------------------------------------//

/// A parse results is a list of AST nodes or a parse error.
pub type ParseResults = Result<Vec<Ast>, ParseError>;

/// A parse result is either an AST or a parse error.
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

    /// Parse a given number of expressions from the stream of tokens.
    /// Returns a global AST or a parse error.
    ///
    /// # Note
    /// If the parser encounters `EOF`, it will **ONLY add empty unit expressions** in the resulting AST.
    pub fn parse_exact(&mut self, count: usize) -> ParseResults {
        let mut ast = Vec::new();
        for _ in 0..count {
            match self.parse_one() {
                //? Ignore empty unit expressions,
                //? add top-level expressions to the global AST anyway
                Ok(expr) => ast.push(expr),
                Err(e) => return Err(e),
            }
        }
        Ok(ast)
    }

    /// Parse a global AST from the stream of tokens.
    /// A global AST is a list of **all** top-level AST nodes (expressions).
    pub fn parse_all(&mut self) -> ParseResults {
        let mut asts = Vec::new();
        loop {
            if let Ok(t) = self.lexer.peek_token_no_nl() {
                if t.token == Token::EndOfFile {
                    break;
                }
            }
            match self.parse_top_expr() {
                Ok(expr) => asts.push(expr),
                Err(e) => return Err(e),
            }
        }
        Ok(asts)
    }

    /// Parse **a single** expression from the stream of tokens.
    /// Returns an AST node or an error.
    /// If the first token is an EOF, then the parser will return an empty unit expression.
    ///
    /// # Note
    /// The parser will not necessarily consume all tokens from the stream.
    /// It will **ONLY** consume a whole complete expression.
    /// There may be remaining tokens in the stream after the expression is parsed.
    pub fn parse_one(&mut self) -> ParseResult {
        // Check if the next token is an EOF, then return an empty unit top-level expression
        if let Ok(t) = self.lexer.peek_token_no_nl() {
            if t.token == Token::EndOfFile {
                return Ok(unit());
            }
        }

        // if let Ok(e) = expr.as_ref() { println!("Parsed expression: {:?}", e); }
        self.parse_top_expr()
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
            let op = op.as_ref().unwrap();
            if op.token.is_terminator() {
                return None;
            }
            op.token
                .get_operator()
                .filter(|op| op.pos() == OperatorPosition::Infix && op.precedence() >= min_prec)
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
                    false
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
        let expr = self.parse_expr(lhs, 0);
        let nt = self.lexer.peek_token(0);
        if let Ok(t) = nt {
            if t.token.is_top_level_terminal(false) {
                self.lexer.read_next_token().unwrap();
            }
        }
        expr
    }
}

//--------------------------------------------------------------------------------------//
//                               Parser Factory Functions                               //
//--------------------------------------------------------------------------------------//

fn setup_new_parser<R: BufRead + Seek>(mut lexer: Lexer<R>) -> Parser<R> {
    init_lexer(&mut lexer);
    Parser::new(lexer)
}

pub fn from_file(file: File, path: &Path) -> Parser<BufReader<File>> {
    setup_new_parser(lexer::from_file(file, path.to_path_buf()))
}

pub fn from_path(source_file: &Path) -> Result<Parser<BufReader<File>>, Error> {
    Ok(setup_new_parser(lexer::from_path(
        source_file.to_path_buf(),
    )?))
}

pub fn from_string(source: String) -> Parser<Cursor<String>> {
    setup_new_parser(lexer::from_string(source))
}

pub fn from_str(source: &str) -> Parser<BytesReader<'_>> {
    setup_new_parser(lexer::from_str(source))
}

pub fn from_stdin() -> Parser<StdinReader> {
    setup_new_parser(lexer::from_stream(
        StdinReader::new(std::io::stdin()),
        "stdin".to_string(),
    ))
}

//--------------------------------------------------------------------------------------//
//                           Direct Parsing Helper Functions                            //
//--------------------------------------------------------------------------------------//

/// Returns a parsed module from a given source file or a parse error.
pub type ModuleResult = Result<Module, ParseError>;

pub fn parse_string_one(source: String) -> ModuleResult {
    Ok(Module::new(
        String::from("unnamed"),
        vec![from_string(source).parse_one()?],
        InputSource::String,
    ))
}

pub fn parse_string_all(source: String) -> ModuleResult {
    Ok(Module::new(
        String::from("unnamed"),
        from_string(source).parse_all()?,
        InputSource::String,
    ))
}

pub fn parse_str_one(source: &str) -> ModuleResult {
    Ok(Module::new(
        String::from("unnamed"),
        vec![from_str(source).parse_one()?],
        InputSource::String,
    ))
}

pub fn parse_str_all(source: &str) -> ModuleResult {
    Ok(Module::new(
        String::from("unnamed"),
        from_str(source).parse_all()?,
        InputSource::String,
    ))
}

pub fn parse_stdin_one() -> ModuleResult {
    Ok(Module::new(
        String::from("unnamed"),
        vec![from_stdin().parse_one()?],
        InputSource::Stream("stdin".to_string()),
    ))
}

pub fn parse_stdin_all() -> ModuleResult {
    Ok(Module::new(
        String::from("unnamed"),
        from_stdin().parse_all()?,
        InputSource::Stream("stdin".to_string()),
    ))
}

pub fn parse_file_one(file: File, path: &Path) -> ModuleResult {
    Ok(Module::new(
        String::from("unnamed"),
        vec![from_file(file, path).parse_one()?],
        InputSource::File(path.to_path_buf()),
    ))
}

pub fn parse_file_all(file: File, path: &Path) -> ModuleResult {
    Ok(Module::new(
        String::from("unnamed"),
        from_file(file, path).parse_all()?,
        InputSource::File(path.to_path_buf()),
    ))
}

pub fn parse_path_one(path: &Path) -> ModuleResult {
    let mut parser = from_path(path).map_err(|e| ParseError {
        message: format!("Failed to open file: {}", e),
    })?;
    Ok(Module::new(
        String::from("unnamed"),
        vec![parser.parse_one()?],
        InputSource::File(path.to_path_buf()),
    ))
}

pub fn parse_path_all(path: &Path) -> ModuleResult {
    let mut parser = from_path(path).map_err(|e| ParseError {
        message: format!("Failed to open file: {}", e),
    })?;
    Ok(Module::new(
        String::from("unnamed"),
        parser.parse_all()?,
        InputSource::File(path.to_path_buf()),
    ))
}
