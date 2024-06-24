use std::{
    fs::File,
    io::{BufReader, Cursor, Error, Read},
    path::Path,
};

use crate::{
    interpreter::value::{Number, Value},
    lexer::{
        lexer::{self, InputSource, LexResult},
        op::{
            Operator, OperatorAssociativity, OperatorPosition, OperatorPrecedence,
            StaticOperatorAst,
        },
        readers::{bytes_reader::BytesReader, stdin::StdinReader},
        token::{TokenInfo, TokenKind},
    },
    stdlib::init::init_lexer,
    type_checker::types::{CheckedType, GetType, Type},
};

use crate::lexer::lexer::Lexer;

use super::{ast::{unit, Ast, Module}, error::ParseError};

/// Token predicates for parsing
mod pred {
    use crate::lexer::token::TokenKind;

    pub fn eof(t: &TokenKind) -> bool {
        matches!(t, TokenKind::EndOfFile)
    }

    /// Check if the token is an ignored token.
    /// These include:
    /// - `Newline`
    /// - `Comment`
    pub fn ignored(t: &TokenKind) -> bool {
        matches!(t, TokenKind::Comment(_) | TokenKind::Newline)
    }
}

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
    R: Read,
{
    lexer: Lexer<R>,
}

impl<R: Read> Parser<R> {
    pub fn new(lexer: Lexer<R>) -> Self {
        Self { lexer }
    }

    pub fn get_lexer(&mut self) -> &mut Lexer<R> {
        &mut self.lexer
    }

    fn index(&self) -> usize {
        self.lexer.current_index()
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
            if let Ok(t) = self.lexer.peek_token_not(pred::ignored) {
                if pred::eof(&t.token) {
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
        if let Ok(t) = self.lexer.peek_token_not(pred::ignored) {
            if pred::eof(&t.token) {
                return Ok(unit());
            }
        }

        // if let Ok(e) = expr.as_ref() { println!("Parsed expression: {:?}", e); }
        self.parse_top_expr()
    }

    fn parse_literal(&mut self, token: TokenKind) -> Option<Value> {
        Some(match token {
            TokenKind::Integer(n) => Value::Number(Number::parse(n)),
            TokenKind::Float(n) => Value::Number(Number::parse(n)),
            TokenKind::String(s) => Value::String(s.clone()),
            TokenKind::Char(c) => Value::Char(c),
            TokenKind::Boolean(b) => Value::Boolean(b),
            _ => return None,
        })
    }

    fn parse_call(&mut self, id: String) -> ParseResult {
        let mut args = Vec::new();
        let nt = self.lexer.read_next_token_not(pred::ignored);
        if nt.is_err() {
            return Err(ParseError {
                message: "Expected '('".to_string(),
                span: (self.index(), self.index()),
            });
        }
        assert!(nt.unwrap().token == TokenKind::LeftParen);
        while let Ok(t) = self.lexer.peek_token(0) {
            if t.token == TokenKind::RightParen {
                break;
            }
            let expr = self.parse_top_expr()?;
            args.push(expr);
            let nt = self.lexer.peek_token(0);
            if nt.is_err() {
                return Err(ParseError {
                    message: "Expected ')'".to_string(),
                    span: (self.index(), self.index()),
                });
            }
            let nt = nt.unwrap().token;
            if nt == TokenKind::RightParen {
                if let Err(err) = self.lexer.read_next_token() {
                    return Err(ParseError {
                        message: format!("Expected ')' but failed with: {}", err.message),
                        span: (self.index(), self.index() + 1),
                    });
                }
                break;
            }
            let nt = self.lexer.read_next_token();
            if nt.is_err() {
                return Err(ParseError {
                    message: "Expected ')'".to_string(),
                    span: (self.index(), self.index() + 1),
                });
            }
            let nt = nt.unwrap().token;
            if nt.is_terminator() {
                continue;
            }
            return Err(ParseError {
                message: "Expected ')'".to_string(),
                span: (self.index(), self.index() + 1),
            });
        }
        // TODO: Extract read_until_terminator() helper method
        Ok(Ast::FunctionCall(id, args, CheckedType::Unchecked))
    }

    fn parse_primary(&mut self) -> ParseResult {
        let nt = self.lexer.expect_next_token_not(pred::ignored);
        if let Ok(t) = nt {
            if t.token.is_literal() {
                Ok(Ast::Literal(self.parse_literal(t.token).unwrap()))
            } else if let TokenKind::Identifier(id) = t.token {
                // Check if function call
                if let Ok(t) = self.lexer.peek_token(0) {
                    if t.token == TokenKind::LeftParen {
                        return self.parse_call(id);
                    }
                }
                Ok(Ast::Identifier(id, CheckedType::Unchecked))
            } else if let TokenKind::TypeIdentifier(t) = t.token {
                Ok(Ast::TypeIdentifier(t, CheckedType::Unchecked))
            } else if t.token.is_operator() {
                let op = t.token.get_operator().unwrap();
                if op.pos() != OperatorPosition::Prefix {
                    return Err(ParseError {
                        message: format!(
                            "Expected optional prefix operator, but found {:?} ({})",
                            op.symbol(),
                            op.name()
                        ),
                        span: (t.info.start.index, t.info.end.index)
                    });
                }
                let rhs = self.parse_primary()?;
                // Ok(Ast::Unary(op, Box::new(rhs), CheckedType::Unchecked))
                Ok(match op {
                    Operator::Runtime(rt) => Ast::Unary(rt, Box::new(rhs), CheckedType::Unchecked),
                    Operator::Static(st) => (st.handler)(StaticOperatorAst::Prefix(rhs)),
                })
            } else if t.token.is_grouping_start() {
                match t.token {
                    TokenKind::LeftParen => {
                        if let Ok(t) = self.lexer.peek_token(0) {
                            if t.token == TokenKind::RightParen {
                                self.lexer.read_next_token().unwrap();
                                return Ok(Ast::Tuple(vec![], CheckedType::Checked(Type::Unit)));
                            }
                        }
                        // Tuples are defined by a comma-separated list of expressions
                        let expr = self.parse_top_expr()?;
                        self.parse_expected(TokenKind::RightParen, ")")?;
                        Ok(expr)
                    }
                    TokenKind::LeftBrace => {
                        let mut exprs = Vec::new();
                        while let Ok(t) = self.lexer.peek_token(0) {
                            if t.token == TokenKind::RightBrace { break; }
                            exprs.push(self.parse_top_expr()?);
                        }
                        self.parse_expected(TokenKind::RightBrace, "}")?;
                        let return_type = if let Some(expr) = exprs.last() {
                            expr.get_type()
                        } else {
                            CheckedType::Checked(Type::Unit)
                        };
                        Ok(Ast::Block(exprs, return_type))
                    }
                    TokenKind::LeftBracket => {
                        let body = self.parse_top_expr()?;
                        self.parse_expected(TokenKind::RightBracket, "]")?;
                        match body {
                            Ast::Tuple(elems, _) => Ok(Ast::List(elems, CheckedType::Unchecked)),
                            single => Ok(Ast::List(vec![single], CheckedType::Unchecked))
                        }
                    }
                    _ => unreachable!()
                }
            } else {
                Err(ParseError {
                    message: format!("Expected primary expression, but found {:?}", t.token),
                    span: (t.info.start.index, t.info.end.index),
                })
            }
        } else {
            Err(ParseError {
                message: format!(
                    "Expected primary expression, but failed due to: {:?}",
                    nt.unwrap_err().message
                ),
                span: (self.index(), self.index()),
            })
        }
    }

    /// Check if the next token is an infix binary operator with a precedence greater than or equal to `min_prec`.
    ///
    /// ## Note
    /// If it is, then return the operator, otherwise return None.
    /// If the next token is a terminator, then return None.
    fn parse_expr_first(op: &LexResult, min_prec: OperatorPrecedence) -> Option<Operator> {
        if let Ok(t) = op {
            if t.token.is_terminator() {
                return None;
            }
            return t
                .token
                .get_operator()
                .filter(|op| op.pos().is_infix() && op.precedence() >= min_prec);
        }
        None
    }

    /// Check if the next token `nt` is an infix binary operator whose precedence
    /// is greater than op's, or a right-associative operator whose precedence is equal to `op`'s.
    /// If it is, then return the operator, otherwise return None.
    ///
    /// ## Note
    /// Return true if either:
    /// - `op` is a binary operator whose precedence is greater than op's
    /// - `op` is a right-associative binary operator whose precedence is equal to op's
    fn parse_expr_next(op: &Operator, nt: &LexResult) -> Option<Operator> {
        let t = if let Ok(t) = nt { t } else { return None };
        let nt_op = if let Some(nt_op) = t.token.get_operator() { nt_op } else { return None };
        let is_infix = nt_op.pos().is_infix();
        let is_greater = nt_op.precedence() > op.precedence();
        let is_right_assoc = nt_op.associativity() == OperatorAssociativity::Right;
        let is_equal = nt_op.precedence() == op.precedence();
        if is_infix && (is_greater || (is_right_assoc && is_equal)) {
            Some(nt_op)
        } else {
            None
        }
    }

    fn next_prec(curr_op: &Operator, next_op: &Operator) -> OperatorPrecedence {
        curr_op.precedence() + (next_op.precedence() > curr_op.precedence()) as OperatorPrecedence
    }

    /// Parse an expression with a given left-hand side and minimum precedence level
    /// using the operator precedence parsing (pratt parsing) algorithm.
    ///
    /// ## Arguments
    /// - `lhs` The left-hand side of the expression
    /// - `min_prec` The minimum precedence of the expression
    ///
    /// ## Returns
    /// The parsed expression or a parse error if the expression could not be parsed
    ///
    /// ## Algorithm
    /// See:
    /// - https://en.wikipedia.org/wiki/Operator-precedence_parser
    /// - https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    /// - https://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing
    /// - https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm
    /// - https://crockford.com/javascript/tdop/tdop.html
    fn parse_expr(&mut self, lhs: Ast, min_prec: OperatorPrecedence) -> ParseResult {
        let mut nt = self.lexer.peek_token(0);
        let mut expr = lhs;
        while let Some(curr_op) = Self::parse_expr_first(&nt, min_prec) {
            self.lexer.read_next_token().unwrap(); // Consume the operator token
            if curr_op.pos().is_accumulate() {
                expr = self.parse_expr_accum(curr_op, expr)?;
                nt = self.lexer.peek_token(0);
                continue;
            }
            let mut rhs = self.parse_primary()?;
            nt = self.lexer.peek_token(0);
            while let Some(next_op) = Self::parse_expr_next(&curr_op, &nt) {
                rhs = self.parse_expr(rhs, Self::next_prec(&curr_op, &next_op))?;
                nt = self.lexer.peek_token(0);
            }
            expr = match curr_op {
                Operator::Runtime(rt) => {
                    Ast::Binary(Box::new(expr), rt, Box::new(rhs), CheckedType::Unchecked)
                }
                Operator::Static(st) => (st.handler)(StaticOperatorAst::Infix(expr, rhs)),
            }
        }
        Ok(expr)
    }

    /// Expect the parser state to be at the end of [expr, op] sequence.
    /// Next token should be a new expression or a terminator.
    fn parse_expr_accum(&mut self, op: Operator, first: Ast) -> ParseResult {
        let mut exprs = vec![first];
        loop {
            // Check if the next token is a terminator, then break the loop
            let nt = self.lexer.peek_token(0);
            if let Ok(nt) = nt {
                if op.allow_trailing() && nt.token.is_terminator() { break; }
            } else {
                break;
            }

            // Parse the next expression in the sequence
            exprs.push(self.parse_primary()?);

            // Expect the next token to be the same operator or another expression
            let nt = self.lexer.peek_token(0);
            if let Some(nt_op) = Self::parse_expr_first(&nt, op.precedence()) {
                if nt_op != op { break; }
                self.lexer.read_next_token().unwrap(); // Consume the operator token
            } else { break; }
        }
        Ok(match op {
            Operator::Runtime(rt) => Ast::VariationCall(rt.handler, exprs, CheckedType::Unchecked),
            Operator::Static(st) => (st.handler)(StaticOperatorAst::Accumulate(exprs)),
        })
    }

    /// Parse a top-level expression.
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

    fn parse_expected(&mut self, expected_token: TokenKind, symbol: &'static str) -> Result<TokenInfo, ParseError>{
        match self.lexer.expect_next_token_not(pred::ignored) {
            Ok(t) if t.token == expected_token => Ok(t),
            Ok(t) => Err(ParseError {
                message: format!("Expected '{}' but found {:?}", symbol, t),
                span: (t.info.start.index, t.info.end.index),
            }),
            Err(err) => Err(ParseError {
                message: format!(
                    "Expected '{}', but failed due to: {:?}",
                    symbol,
                    err.message
                ),
                span: (self.index(), self.index()),
            })
        }
    }
}

//--------------------------------------------------------------------------------------//
//                               Parser Factory Functions                               //
//--------------------------------------------------------------------------------------//

fn setup_new_parser<R: Read>(mut lexer: Lexer<R>) -> Parser<R> {
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
        StdinReader::default(),
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
        span: (0, 0),
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
        span: (0, 0),
    })?;
    Ok(Module::new(
        String::from("unnamed"),
        parser.parse_all()?,
        InputSource::File(path.to_path_buf()),
    ))
}
