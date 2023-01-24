use std::collections::HashMap;

use crate::{lexer::op::RuntimeOperator, type_checker::types::{Type, FunctionParameterType}};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RecordKeyAst {
    String(String),
    Integer(String),
    Float(String),
    Char(char),
}

type CheckedType = Option<Type>;

/**
 * The AST is a tree of nodes that represent the program.
 * All nodes are expressions, and the root node is the program itself.
 * The AST is generated by the parser, and then interpreted by the interpreter module or compiled.
 */
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ast {
    Integer(String, CheckedType),
    Float(String, CheckedType),
    String(String),
    Char(char),
    Identifier(String, CheckedType),
    TypeIdentifier(String, CheckedType),
    FunctionCall(String, Vec<Ast>, CheckedType),
    Function(String, FunctionParameterType, Box<Ast>, CheckedType),
    Tuple(Vec<Ast>, CheckedType),
    List(Vec<Ast>, CheckedType),
    Record(HashMap<RecordKeyAst, Ast>, CheckedType),
    Binary(Box<Ast>, RuntimeOperator, Box<Ast>, CheckedType),
    Unary(RuntimeOperator, Box<Ast>, CheckedType),
    Assignment(Box<Ast>, Box<Ast>, CheckedType),
    /**
     * Block expression evaluates all expressions in the block and returns the value of the last expression.
     */
    Block(Vec<Ast>, CheckedType),
}

/**
 * Create a new unit AST node.
 * Implemented as a tuple with no elements.
 */
pub fn unit() -> Ast {
    Ast::Tuple(vec![], Some(Type::Unit))
}
