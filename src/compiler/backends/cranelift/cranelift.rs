use std::{io::Write, sync::Arc};

use cranelift_codegen::{isa, settings};

use crate::{
    compiler::compiler::{Backend, CompileOptions, CompileResult},
    parser::ast::Module,
};

pub struct Cranelift {
    isa: Arc<dyn isa::TargetIsa>,
    flags: settings::Flags,
}

impl Cranelift {
    pub fn new(isa: Arc<dyn isa::TargetIsa>, flags: settings::Flags) -> Self {
        Self { isa, flags }
    }
}

impl<Out: Write> Backend<Out> for Cranelift {
    fn compile_module(&mut self, module: &Module, options: CompileOptions<Out>) -> CompileResult {
        todo!()
    }
}
