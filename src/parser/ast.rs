use crate::{
    interpreter::value::{RecordKey, Value},
    lexer::lexer::InputSource,
    type_checker::types::{FunctionParameterType, Type},
};

use super::op::OperatorInfo;

/// The AST is a tree of nodes that represent the program.
/// All nodes are expressions, and the root node is the program itself.
/// The AST is generated by the parser, and then interpreted by the interpreter module or compiled.
#[derive(Debug, Clone, PartialEq)]
pub enum Ast {
    /// A literal is a constant value that is directly represented in the source code.
    /// 1. Value of the literal
    Literal(Value),
    /// A tuple is a fixed-size collection of elements of possibly different types.
    /// 1. List of elements
    Tuple(Vec<Ast>),
    /// A dynamic list of elements.
    /// 1. List of elements
    /// 2. Type of every element in the list (all elements must **be a subtype**)
    List(Vec<Ast>),
    /// A record is a collection of key-value pairs
    /// 1. List of key-value pairs
    Record(Vec<(RecordKey, Ast)>),
    /// An identifier is a named reference to a value in the environment
    /// 1. Name of the identifier
    Identifier(String),
    /// A function call is an invocation of a function with a list of arguments
    /// 1. Name of the function
    /// 2. List of arguments
    FunctionCall(String, Vec<Ast>),
    /// A function declaration is a named function with a list of parameters and a body expression
    Function {
        // name: String,
        params: FunctionParameterType,
        body: Box<Ast>,
        return_type: Option<Type>,
    },
    /// An accumulate expression is an operation with multiple operands
    /// 1. Operator
    /// 2. List of operands
    Accumulate(OperatorInfo, Vec<Ast>),
    /// A binary expression is an operation with two operands
    /// 1. Left operand
    /// 2. Operator
    /// 3. Right operand
    Binary(Box<Ast>, OperatorInfo, Box<Ast>),
    /// A unary expression is an operation with one operand
    /// 1. Operator
    /// 2. Operand
    Unary(OperatorInfo, Box<Ast>),
    /// An assignment expression assigns a value to a variable
    /// 1. Matching pattern (identifier, destructuring of a tuple, record, etc.)
    /// 2. Value
    Assignment(Box<Ast>, Box<Ast>),
    /// Block expression evaluates all expressions in the block and returns the value of the last expression.
    /// 1. List of expressions
    Block(Vec<Ast>),
}

impl Ast {
    pub fn print_sexpr(&self) -> String {
        match self {
            Ast::Literal(value) => value.to_string(),
            Ast::Tuple(elements) => format!(
                "({})",
                elements
                    .iter()
                    .map(|e| e.print_sexpr())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            Ast::List(elements) => format!(
                "[{}]",
                elements
                    .iter()
                    .map(|e| e.print_sexpr())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            Ast::Record(_elements) => todo!(),
            Ast::Identifier(name) => name.clone(),
            Ast::FunctionCall(name, args) => format!(
                "({} {})",
                name,
                args.iter()
                    .map(|e| e.print_sexpr())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            // Ast::VariationCall(variation, args) => format!(
            //     "({} {})",
            //     variation,
            //     args.iter()
            //         .map(|e| e.print_sexpr())
            //         .collect::<Vec<String>>()
            //         .join(" ")
            // ),
            Ast::Function { params, body, .. } => {
                format!("({} -> {})", params, body.print_sexpr())
            }
            Ast::Accumulate(op, operands) => {
                format!(
                    "({} {})",
                    op.symbol.clone(),
                    operands
                        .iter()
                        .map(|e| e.print_sexpr())
                        .collect::<Vec<String>>()
                        .join(" ")
                )
            }
            Ast::Binary(lhs, op, rhs) => format!(
                "({} {} {})",
                op.symbol.clone(),
                lhs.print_sexpr(),
                rhs.print_sexpr()
            ),
            Ast::Unary(op, operand) => {
                format!("({} {})", op.symbol.clone(), operand.print_sexpr())
            }
            Ast::Assignment(lhs, rhs) => {
                format!("(= {} {})", lhs.print_sexpr(), rhs.print_sexpr())
            }
            Ast::Block(expressions) => format!(
                "({})",
                expressions
                    .iter()
                    .map(|e| e.print_sexpr())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
        }
    }
}

/// Module is the root program node of the AST
/// It contains a list of all the expressions in the program
#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub expressions: Vec<Ast>,
    pub source: InputSource,
}

impl Module {
    pub fn new(name: String, expressions: Vec<Ast>, source: InputSource) -> Module {
        Module {
            name,
            expressions,
            source,
        }
    }
}
