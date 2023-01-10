
#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    Print(String),
    String(&'static str),
    Unit,
}

#[derive(Debug, Clone)]
pub struct Ast {
    pub name: String,
    pub value: Box<AstNode>
}
