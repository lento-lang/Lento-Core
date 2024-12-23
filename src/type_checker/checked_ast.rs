use crate::{
    interpreter::value::{FunctionVariation, RecordKey, Value},
    lexer::lexer::InputSource,
    type_checker::types::{FunctionParameterType, Type},
};

use super::types::{GetType, VariationType};

#[derive(Debug, Clone)]
pub struct CheckedOperator {
    pub name: String,
    pub symbol: String,
    pub handler: FunctionVariation,
}

/// The AST is a tree of nodes that represent the program.
/// All nodes are expressions, and the root node is the program itself.
/// The AST is generated by the parser, and then interpreted by the interpreter module or compiled.
#[derive(Debug, Clone)]
pub enum CheckedAst {
    /// A literal is a constant value that is directly represented in the source code.
    /// 1. Value of the literal
    Literal(Value),
    /// A tuple is a fixed-size collection of elements of possibly different types.
    /// 1. List of elements
    /// 2. Type of the tuple, made up of the types of the elements and the number of elements
    Tuple(Vec<CheckedAst>, Type),
    /// A dynamic list of elements.
    /// 1. List of elements
    /// 2. Type of every element in the list (all elements must **be a subtype**)
    List(Vec<CheckedAst>, Type),
    /// A record is a collection of key-value pairs
    /// 1. List of key-value pairs
    /// 2. Type of the record, made up of the types of the keys and values
    Record(Vec<(RecordKey, CheckedAst)>, Type),
    /// An identifier is a named reference to a value in the environment
    /// 1. Name of the identifier
    /// 2. Type of the identifier (the type of the value it refers to)
    Identifier(String, Type),
    /// A function variation call is an invocation of a function variation with a list of arguments
    Call {
        function: String,
        variation: Box<VariationType>,
        args: Vec<CheckedAst>,
    },
    /// A direct function call is an invocation of a variation with a list of arguments
    DirectCall {
        variation: Box<FunctionVariation>,
        args: Vec<CheckedAst>,
    },
    /// A function declaration is a named function with a list of parameters and a body expression
    Function {
        params: FunctionParameterType,
        body: Box<CheckedAst>,
        return_type: Type,
    },
    /// An assignment expression assigns a value to a variable
    /// 1. Matching pattern (identifier, destructuring of a tuple, record, etc.)
    /// 2. Value
    /// 3. Type of the value
    Assignment(Box<CheckedAst>, Box<CheckedAst>, Type),
    /// Block expression evaluates all expressions in the block and returns the value of the last expression.
    /// 1. List of expressions
    /// 2. Type of the last expression
    Block(Vec<CheckedAst>, Type),
}

impl GetType for CheckedAst {
    fn get_type(&self) -> &Type {
        match self {
            CheckedAst::Literal(v) => v.get_type(),
            CheckedAst::Tuple(_, ty) => ty,
            CheckedAst::List(_, ty) => ty,
            CheckedAst::Record(_, ty) => ty,
            CheckedAst::Identifier(_, ty) => ty,
            CheckedAst::Call { variation, .. } => variation.get_return_type(),
            CheckedAst::DirectCall {
                variation: function,
                ..
            } => function.get_return_type(),
            CheckedAst::Function { body, .. } => body.get_type(),
            CheckedAst::Assignment(_, _, ty) => ty,
            CheckedAst::Block(_, ty) => ty,
        }
    }
}

impl CheckedAst {
    pub fn print_sexpr(&self) -> String {
        match self {
            CheckedAst::Literal(value) => value.to_string(),
            CheckedAst::Tuple(elements, _) => format!(
                "({})",
                elements
                    .iter()
                    .map(|e| e.print_sexpr())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            CheckedAst::List(elements, _) => format!(
                "[{}]",
                elements
                    .iter()
                    .map(|e| e.print_sexpr())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            CheckedAst::Record(_elements, _) => todo!(),
            CheckedAst::Identifier(name, _) => name.clone(),
            CheckedAst::Call {
                variation, args, ..
            } => format!(
                "[{}]({})",
                variation,
                args.iter()
                    .map(|e| e.print_sexpr())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            CheckedAst::DirectCall {
                variation: function,
                args,
                ..
            } => format!(
                "[{}]({})",
                function,
                args.iter()
                    .map(|e| e.print_sexpr())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            CheckedAst::Function { params, body, .. } => {
                format!("({} -> {})", params, body.print_sexpr())
            }
            CheckedAst::Assignment(lhs, rhs, _) => {
                format!("({} = {})", lhs.print_sexpr(), rhs.print_sexpr())
            }
            CheckedAst::Block(expressions, _) => format!(
                "{{{}}}",
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
pub struct CheckedModule {
    pub name: String,
    pub expressions: Vec<CheckedAst>,
    pub source: InputSource,
}

impl CheckedModule {
    pub fn new(name: String, expressions: Vec<CheckedAst>, source: InputSource) -> CheckedModule {
        CheckedModule {
            name,
            expressions,
            source,
        }
    }
}
