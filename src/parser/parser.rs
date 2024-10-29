use std::{
    collections::HashMap,
    fs::File,
    io::{BufReader, Cursor, Error, Read},
    path::Path,
};

use crate::{
    interpreter::value::{Number, Value},
    lexer::{
        lexer::{self, InputSource, LexResult},
        readers::{bytes_reader::BytesReader, stdin::StdinReader},
        token::{TokenInfo, TokenKind},
    },
    stdlib::init::stdlib,
    type_checker::types::{CheckedType, GetType, Type},
    util::failable::Failable,
};

use crate::lexer::lexer::Lexer;

use super::{
    ast::{unit, Ast, Module, RecordKeyAst},
    error::{OperatorError, ParseError, TypeError},
    op::{
        Operator, OperatorAssociativity, OperatorHandler, OperatorPosition, OperatorPrecedence,
        RuntimeOperator, StaticOperatorAst,
    },
};

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
    /// A map of all defined operators in the parser indexed by their symbol.
    ///
    /// ## Note
    /// The parser will allow redefining operators with the same symbol **only if**:
    /// - They have different signatures
    /// - They have different positions
    /// - The symbol is a built-in operator that is overloadable
    operators: HashMap<String, Vec<Operator>>,
    /// A map of all defined types in the parser.
    ///
    /// ## Note
    /// This is used to lookup types during parse-time type-inference
    /// and to prevent redefining types.
    types: HashMap<String, Type>,
}

impl<R: Read> Parser<R> {
    pub fn new(lexer: Lexer<R>) -> Self {
        Self {
            lexer,
            operators: HashMap::new(),
            types: HashMap::new(),
        }
    }

    fn index(&self) -> usize {
        self.lexer.current_index()
    }

    fn reset(&mut self, reader: R) {
        self.lexer.reset(reader);
    }

    /// Define an operator in the parser.
    /// If the operator already exists with the same signature,
    pub fn define_op(&mut self, op: Operator) -> Failable<OperatorError> {
        if let Some(existing) = self.get_op(&op.symbol) {
            if existing.iter().any(|e| e.signature() == op.signature()) {
                return Err(OperatorError::SignatureForSymbolExists);
            }
            if existing.iter().any(|e| e.position == op.position) {
                return Err(OperatorError::PositionForSymbolExists);
            }
            if !op.overloadable && existing.iter().any(|e| !e.overloadable) {
                return Err(OperatorError::SymbolNotOverloadable);
            }
        }
        self.lexer.operators.insert(op.symbol.clone());
        self.operators
            .entry(op.symbol.clone())
            .or_default()
            .push(op);
        Ok(())
    }

    pub fn get_op(&self, symbol: &str) -> Option<&Vec<Operator>> {
        self.operators.get(symbol)
    }

    pub fn find_operator(
        &self,
        symbol: &str,
        pred: impl Fn(&Operator) -> bool,
    ) -> Option<&Operator> {
        self.get_op(symbol)
            .and_then(|ops| ops.iter().find(|op| pred(op)))
    }

    pub fn find_operator_pos(&self, symbol: &str, pos: OperatorPosition) -> Option<&Operator> {
        self.find_operator(symbol, |op| op.position == pos)
    }

    pub fn define_literal_type(&mut self, ty: Type) -> Failable<TypeError> {
        if let Type::Literal(name) = ty.clone() {
            if self.types.contains_key(&name.to_string()) {
                return Err(TypeError::TypeExists);
            }
            self.types.insert(name.to_string(), ty);
            self.lexer.types.insert(name.to_string());
            Ok(())
        } else {
            Err(TypeError::NonLiteralType)
        }
    }

    pub fn define_alias_type(&mut self, name: String, ty: Type) -> Failable<TypeError> {
        if self.types.contains_key(&name) {
            return Err(TypeError::TypeExists);
        }
        self.types.insert(name.clone(), ty);
        self.lexer.types.insert(name);
        Ok(())
    }

    pub fn get_type(&self, name: &str) -> Option<&Type> {
        self.types.get(name)
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
                if let Err(err) = self.lexer.next_token() {
                    return Err(ParseError {
                        message: format!("Expected ')' but failed with: {}", err.message),
                        span: (self.index(), self.index() + 1),
                    });
                }
                break;
            }
            let nt = self.lexer.next_token();
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

    /// Parses the fields of a record from the lexer.
    ///
    /// This function attempts to parse a record by first performing a soft parse to check if the record
    /// is empty or if it is a block. If a valid key and a colon are found, it continues to parse the
    /// fields more strictly.
    ///
    /// # Returns
    /// - `Some(Ok(Vec<(RecordKeyAst, Ast)>))` if the record is successfully parsed.
    /// - `Some(Err(ParseError))` if there is an error during parsing.
    /// - `None` if the input does not represent a record.
    ///
    /// # Errors
    /// This function returns a `ParseError` if it encounters unexpected tokens or if it fails to parse
    /// the expected tokens.
    ///
    /// # Examples
    /// ```
    /// let mut parser = Parser::new(lexer);
    /// if let Some(result) = parser.parse_record_fields() {
    ///     match result {
    ///         Ok(fields) => println!("Parsed fields: {:?}", fields),
    ///         Err(err) => eprintln!("Parse error: {:?}", err),
    ///     }
    /// } else {
    ///     println!("Not a record.");
    /// }
    /// ```
    fn parse_record_fields(&mut self) -> Option<Result<Vec<(RecordKeyAst, Ast)>, ParseError>> {
        let mut fields = Vec::new();
        // Initial soft parse to check if the record is empty
        // Or if it is a block
        if let Ok(t) = self.lexer.peek_token(0) {
            let key = match t.token {
                TokenKind::RightBrace => {
                    self.lexer.next_token().unwrap();
                    return Some(Ok(fields)); // Empty record
                }
                TokenKind::Identifier(id) => RecordKeyAst::String(id),
                TokenKind::Integer(n) => RecordKeyAst::Integer(n),
                TokenKind::Float(n) => RecordKeyAst::Float(n),
                TokenKind::String(s) => RecordKeyAst::String(s),
                TokenKind::Char(c) => RecordKeyAst::Char(c),
                _ => return None, // Not a record
            };
            if let Ok(t) = self.lexer.peek_token(1) {
                if t.token != TokenKind::Colon {
                    return None; // Not a record
                }
            }
            // If we found both a valid key and a colon, we found a record!
            self.lexer.next_token().unwrap();
            self.parse_expected(TokenKind::Colon, ":").ok()?;
            let value = self.parse_top_expr().ok()?;
            fields.push((key, value));
            if let Ok(t) = self.lexer.next_token() {
                match t.token {
                    TokenKind::Comma => (),                           // Continue parsing
                    TokenKind::RightBrace => return Some(Ok(fields)), // Just a single field
                    _ => {
                        return Some(Err(ParseError {
                            message: format!("Expected ',' or '}}', but found {:?}", t),
                            span: (t.info.start.index, t.info.end.index),
                        }))
                    }
                }
            }
        }
        // Parse the rest of the fields more strictly
        while let Ok(t) = self.lexer.next_token() {
            if t.token == TokenKind::RightBrace {
                break;
            }
            let key = match t.token {
                TokenKind::Identifier(id) => RecordKeyAst::String(id),
                TokenKind::Integer(n) => RecordKeyAst::Integer(n),
                TokenKind::Float(n) => RecordKeyAst::Float(n),
                TokenKind::String(s) => RecordKeyAst::String(s),
                TokenKind::Char(c) => RecordKeyAst::Char(c),
                _ => {
                    return Some(Err(ParseError {
                        message: format!("Expected record key, but found {:?}", t.token),
                        span: (t.info.start.index, t.info.end.index),
                    }))
                }
            };
            let _ = Some(self.parse_expected(TokenKind::Colon, ":"))?;
            let value = match self.parse_top_expr() {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            };
            fields.push((key, value));
            if let Ok(t) = self.lexer.next_token() {
                match t.token {
                    TokenKind::Comma => continue,
                    TokenKind::RightBrace => break,
                    _ => {
                        return Some(Err(ParseError {
                            message: format!("Expected ',' or '}}', but found {:?}", t),
                            span: (t.info.start.index, t.info.end.index),
                        }))
                    }
                }
            }
        }
        Some(Ok(fields))
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
            } else if let TokenKind::Op(op) = t.token {
                if let Some(op) = self.find_operator_pos(&op, OperatorPosition::Prefix) {
                    let op = op.clone();
                    let rhs = self.parse_primary()?;
                    Ok(if let OperatorHandler::Static(_, handler) = op.handler {
                        (handler)(StaticOperatorAst::Prefix(rhs))
                    } else {
                        Ast::Unary(
                            RuntimeOperator::from(op.clone()),
                            Box::new(rhs),
                            CheckedType::Checked(op.signature().returns),
                        )
                    })
                } else {
                    return Err(ParseError {
                        message: format!("Expected prefix operator, but found {:?}", op),
                        span: (t.info.start.index, t.info.end.index),
                    });
                }
            } else if t.token.is_grouping_start() {
                match t.token {
                    TokenKind::LeftParen => {
                        if let Ok(t) = self.lexer.peek_token(0) {
                            if t.token == TokenKind::RightParen {
                                self.lexer.next_token().unwrap();
                                return Ok(Ast::Tuple(vec![], CheckedType::Checked(Type::Unit)));
                            }
                        }
                        // Tuples are defined by a comma-separated list of expressions
                        let expr = self.parse_top_expr()?;
                        self.parse_expected(TokenKind::RightParen, ")")?;
                        Ok(expr)
                    }
                    TokenKind::LeftBrace => {
                        // Try to parse as record
                        if let Some(res) = self.parse_record_fields() {
                            Ok(Ast::Record(res?, CheckedType::Unchecked))
                        } else {
                            // Parse as block
                            let mut exprs = Vec::new();
                            while let Ok(t) = self.lexer.peek_token(0) {
                                if t.token == TokenKind::RightBrace {
                                    break;
                                }
                                exprs.push(self.parse_top_expr()?);
                            }
                            self.parse_expected(TokenKind::RightBrace, "}")?;
                            let return_type = exprs.last().unwrap().get_type();
                            Ok(Ast::Block(exprs, return_type))
                        }
                    }
                    TokenKind::LeftBracket => {
                        let body = self.parse_top_expr()?;
                        self.parse_expected(TokenKind::RightBracket, "]")?;
                        match body {
                            Ast::Tuple(elems, _) => Ok(Ast::List(elems, CheckedType::Unchecked)),
                            single => Ok(Ast::List(vec![single], CheckedType::Unchecked)),
                        }
                    }
                    _ => unreachable!(),
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

    /// Check if to continue parsing the next expression in the sequence
    /// based on the precedence of the next operator.
    ///
    /// ## Returns
    /// - `Some(op)`: If the next token is an infix binary operator that either:
    ///     - has a precedence **greater than** `min_prec`
    ///     - is **right-associative** with a precedence **greater than or equal** to `min_prec`
    ///     - `allow_eq` is `true` and precedence **equal** to `min_prec`
    /// - `None`: If the next token is either:
    ///     - **not an infix operator**
    ///     - its **precedence is lower than** `min_prec`
    ///     - it is a **terminator**
    fn check_op(
        &self,
        nt: &LexResult,
        min_prec: OperatorPrecedence,
        allow_eq: bool,
    ) -> Option<Operator> {
        let t = nt.as_ref().ok()?;
        let op = if let TokenKind::Op(op) = &t.token {
            op
        } else {
            return None;
        };
        let op = self.find_operator(op, |op| {
            op.position == OperatorPosition::Infix
                || op.position == OperatorPosition::InfixAccumulate
        })?;
        let is_infix = op.position.is_infix();
        let is_greater = op.precedence > min_prec;
        let is_right_assoc = op.associativity == OperatorAssociativity::Right;
        let is_equal = op.precedence == min_prec;
        if is_infix && (is_greater || ((is_right_assoc || allow_eq) && is_equal)) {
            Some(op.clone())
        } else {
            None
        }
    }

    fn next_prec(curr_op: &Operator, next_op: &Operator) -> OperatorPrecedence {
        curr_op.precedence + (next_op.precedence > curr_op.precedence) as OperatorPrecedence
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
        let mut expr = lhs;
        while let Some(curr_op) = {
            let nt = self.lexer.peek_token(0);
            self.check_op(&nt, min_prec, false)
        } {
            self.lexer.next_token().unwrap(); // Consume the operator token
            if curr_op.position.is_accumulate() {
                expr = self.parse_expr_accum(&curr_op, expr)?;
                continue;
            }
            let mut rhs = self.parse_primary()?;
            while let Some(next_op) = {
                let nt = self.lexer.peek_token(0);
                self.check_op(&nt, curr_op.precedence, false)
            } {
                rhs = self.parse_expr(rhs, Self::next_prec(&curr_op, &next_op))?;
            }
            expr = if let OperatorHandler::Static(_, handler) = curr_op.handler {
                (handler)(StaticOperatorAst::Infix(expr, rhs))
            } else {
                Ast::Binary(
                    Box::new(expr),
                    RuntimeOperator::from(curr_op.clone()),
                    Box::new(rhs),
                    CheckedType::Checked(curr_op.signature().returns),
                )
            };
        }
        Ok(expr)
    }

    /// Expect the parser state to be at the end of [expr, op] sequence.
    /// Next token should be a new expression or a terminator.
    fn parse_expr_accum(&mut self, op: &Operator, first: Ast) -> ParseResult {
        let mut exprs = vec![first];
        while let Ok(t) = self.lexer.peek_token_not(pred::ignored) {
            if op.allow_trailing && t.token.is_terminator() {
                break;
            }

            // Parse the next nested expression in the sequence
            let lhs = self.parse_primary()?;
            exprs.push(self.parse_expr(lhs, op.precedence)?);

            // Expect the next token to be the same operator or another expression
            let nt = self.lexer.peek_token_not(pred::ignored);
            if let Some(next_op) = self.check_op(&nt, op.precedence, true) {
                if !next_op.eq(op) {
                    break;
                }
                self.lexer.read_next_token_not(pred::ignored).unwrap(); // Consume the operator token
            } else {
                break;
            }
        }
        Ok(match &op.handler {
            OperatorHandler::Static(_, handler) => (handler)(StaticOperatorAst::Accumulate(exprs)),
            OperatorHandler::Runtime(func) => {
                Ast::VariationCall(func.clone(), exprs, CheckedType::Unchecked)
            }
        })
    }

    /// Parse a top-level expression.
    fn parse_top_expr(&mut self) -> ParseResult {
        let lhs = self.parse_primary()?;
        let expr = self.parse_expr(lhs, 0);
        let nt = self.lexer.peek_token(0);
        if let Ok(t) = nt {
            if t.token.is_top_level_terminal(false) {
                self.lexer.next_token().unwrap();
            }
        }
        expr
    }

    fn parse_expected(
        &mut self,
        expected_token: TokenKind,
        symbol: &'static str,
    ) -> Result<TokenInfo, ParseError> {
        match self.lexer.expect_next_token_not(pred::ignored) {
            Ok(t) if t.token == expected_token => Ok(t),
            Ok(t) => Err(ParseError {
                message: format!("Expected '{}' but found {:?}", symbol, t),
                span: (t.info.start.index, t.info.end.index),
            }),
            Err(err) => Err(ParseError {
                message: format!(
                    "Expected '{}', but failed due to: {:?}",
                    symbol, err.message
                ),
                span: (self.index(), self.index()),
            }),
        }
    }
}

//--------------------------------------------------------------------------------------//
//                               Parser Factory Functions                               //
//--------------------------------------------------------------------------------------//

fn parser_with_stdlib<R: Read>(lexer: Lexer<R>) -> Parser<R> {
    let mut parser = Parser::new(lexer);
    stdlib().init_parser(&mut parser);
    parser
}

pub fn from_file(file: File, path: &Path) -> Parser<BufReader<File>> {
    parser_with_stdlib(lexer::from_file(file, path.to_path_buf()))
}

pub fn from_path(source_file: &Path) -> Result<Parser<BufReader<File>>, Error> {
    Ok(parser_with_stdlib(lexer::from_path(
        source_file.to_path_buf(),
    )?))
}

pub fn from_string(source: String) -> Parser<Cursor<String>> {
    parser_with_stdlib(lexer::from_string(source))
}

pub fn from_str(source: &str) -> Parser<BytesReader<'_>> {
    parser_with_stdlib(lexer::from_str(source))
}

pub fn from_stdin() -> Parser<StdinReader> {
    parser_with_stdlib(lexer::from_stream(
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
