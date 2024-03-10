use cranelift_codegen::{isa, settings};

use crate::{
    compiler::compiler::{Backend, CompileOptions, CompileResult},
    parser::ast::Module,
};

struct Cranelift {
    isa: Box<dyn isa::TargetIsa>,
    flags: settings::Flags,
}

impl Cranelift {
    pub fn new(isa: Box<dyn isa::TargetIsa>, flags: settings::Flags) -> Self {
        Self { isa, flags }
    }
}

impl Backend for Cranelift {
    fn compile_module(&mut self, module: &Module, options: CompileOptions) -> CompileResult {
        todo!()
    }
}
