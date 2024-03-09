use std::collections::HashMap;

use crate::{
    interpreter::value::Value,
    lexer::{lexer::InputSource, op::Operator},
    type_checker::types::{CheckedType, FunctionParameterType, GetType, Type},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RecordKeyAst {
    String(String),
    Integer(String),
    Float(String),
    Char(char),
}

/// Module is the root program node of the AST
/// It contains a list of all the expressions in the program
pub struct Module {
    pub expressions: Vec<Ast>,
    pub source: InputSource,
}

impl Module {
    pub fn new(expressions: Vec<Ast>, source: InputSource) -> Module {
        Module {
            expressions,
            source,
        }
    }
}

/**
 * The AST is a tree of nodes that represent the program.
 * All nodes are expressions, and the root node is the program itself.
 * The AST is generated by the parser, and then interpreted by the interpreter module or compiled.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum Ast {
    /// A literal is a constant value that is directly represented in the source code.
    /// 1. Value of the literal
    Literal(Value),
    /// A tuple is a fixed-size collection of elements of possibly different types.
    /// 1. List of elements
    /// 2. Type of the tuple, made up of the types of the elements and the number of elements
    Tuple(Vec<Ast>, CheckedType),
    /// A dynamic list of elements.
    /// 1. List of elements
    /// 2. Type of every element in the list (all elements must **be a subtype**)
    List(Vec<Ast>, CheckedType),
    /// A record is a collection of key-value pairs
    /// 1. List of key-value pairs
    /// 2. Type of the record, made up of the types of the keys and values
    Record(HashMap<RecordKeyAst, Ast>, CheckedType),
    /// An identifier is a named reference to a value in the environment
    /// 1. Name of the identifier
    /// 2. Type of the identifier (the type of the value it refers to)
    Identifier(String, CheckedType),
    /// A type identifier is a named reference to a type in the environment
    /// 1. Name of the type identifier
    /// 2. Type definition of the type identifier
    TypeIdentifier(String, CheckedType),
    /// A function call is an invocation of a function with a list of arguments
    /// 1. Name of the function
    /// 2. List of arguments
    /// 3. Type of the return value of the function
    FunctionCall(String, Vec<Ast>, CheckedType),
    /// A function definition is a named function with a list of parameters and a body expression
    /// 1. Name of the function
    /// 2. List of parameters
    /// 3. Body expression
    /// 4. Type of the return value of the function
    Function(String, FunctionParameterType, Box<Ast>, CheckedType),
    /// A binary expression is an operation with two operands
    /// 1. Left operand
    /// 2. Operator
    /// 3. Right operand
    /// 4. Type of the result of the operation
    Binary(Box<Ast>, Operator, Box<Ast>, CheckedType),
    /// A unary expression is an operation with one operand
    /// 1. Operator
    /// 2. Operand
    /// 3. Type of the result of the operation
    Unary(Operator, Box<Ast>, CheckedType),
    /// An assignment expression assigns a value to a variable
    /// 1. Matching pattern (identifier, destructuring of a tuple, record, etc.)
    /// 2. Value
    /// 3. Type of the value
    Assignment(Box<Ast>, Box<Ast>, CheckedType),
    /// Block expression evaluates all expressions in the block and returns the value of the last expression.
    /// 1. List of expressions
    /// 2. Type of the last expression
    Block(Vec<Ast>, CheckedType),
}

impl GetType for Ast {
    fn get_type(&self) -> CheckedType {
        CheckedType::Checked(match self {
            Ast::Literal(value) => return value.get_type(),
            Ast::Tuple(_, CheckedType::Checked(t)) => t.clone(),
            Ast::List(_, CheckedType::Checked(t)) => t.clone(),
            Ast::Record(_, CheckedType::Checked(t)) => t.clone(),
            Ast::Identifier(_, CheckedType::Checked(t)) => t.clone(),
            Ast::TypeIdentifier(_, CheckedType::Checked(t)) => t.clone(),
            Ast::FunctionCall(_, _, CheckedType::Checked(t)) => t.clone(),
            Ast::Function(_, _, _, CheckedType::Checked(t)) => t.clone(),
            Ast::Binary(_, _, _, CheckedType::Checked(t)) => t.clone(),
            Ast::Unary(_, _, CheckedType::Checked(t)) => t.clone(),
            Ast::Assignment(_, _, CheckedType::Checked(t)) => t.clone(),
            Ast::Block(_, CheckedType::Checked(t)) => t.clone(),
            _ => return CheckedType::Unchecked,
        })
    }
}

pub fn tuple(elements: Vec<Ast>) -> Ast {
    Ast::Tuple(elements, CheckedType::Unchecked)
}

/**
 * Create a new unit AST node.
 * Implemented as a tuple with no elements.
 */
pub fn unit() -> Ast {
    Ast::Tuple(vec![], CheckedType::Checked(Type::Unit))
}
