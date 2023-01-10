use crate::parser::ast::{Ast, AstNode};

pub fn interpret_ast(ast: &Ast) -> Result<(), (u8, String)> {
    match ast.value.as_ref() {
        AstNode::Print(msg) => println!("{}", msg),
        AstNode::Unit => (),
        _ => todo!("Implement other AST nodes")
    }
    Ok(())
}
