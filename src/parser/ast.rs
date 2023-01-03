
#[derive(Debug, Clone)]
pub enum AstNode {
    Root(Ast),
    Print(String),
}

#[derive(Debug, Clone)]
pub struct Ast {
    pub name: String,
    pub value: Box<AstNode>
}
