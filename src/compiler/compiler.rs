use std::path::PathBuf;

use crate::{lexer::lexer::InputSource, parser::ast::Module};

use super::error::CompileError;

use target_lexicon::Triple;

//--------------------------------------------------------------------------------------//
//                                      Compiler                                        //
//--------------------------------------------------------------------------------------//

/// Generic *(backend-agnostic)* options for the compiler. \
/// The options are used to configure the compiler's behavior
/// when compiling a module.
pub struct CompileOptions {
    pub optimization_level: u8,
    pub debug_info: bool,
    pub target: Triple,
    pub output_file: PathBuf,
    pub input_source: InputSource,
}

/// The result of compiling a module. \
/// If the module compiles successfully, the result is `Ok(())`. \
/// If the module fails to compile, the result is `Err(CompileError)`.
pub type CompileResult = Result<(), CompileError>;

/// The backend trait is implemented by all compiler backends. \
/// A backend is responsible for compiling a module into, for example:
/// - Machine code (native executable)
/// - Bytecode (virtual machine code)
/// - Intermediate representation (IR)
/// - Text (assembly code)
/// - Translation to another languages such as:
///     - C
///     - JavaScript
///     - etc.
pub trait Backend {
    fn compile_module(&mut self, module: &Module, options: CompileOptions) -> CompileResult
    where
        Self: Sized;
}

/// Compiles a module using the specified backend and options. \
pub fn compile<B: Backend>(
    backend: &mut B,
    module: &Module,
    options: CompileOptions,
) -> CompileResult {
    backend.compile_module(module, options)
}
