use std::path::PathBuf;

use crate::{lexer::lexer::InputSource, parser::ast::Module};

use super::error::CompileError;

//--------------------------------------------------------------------------------------//
//                                      Compiler                                        //
//--------------------------------------------------------------------------------------//

/// The target architecture to compile for. \
/// The target architecture is a string that specifies the target platform. \
/// For example: `x86_64-unknown-linux-gnu`, `aarch64-unknown-linux-gnu`, etc.
pub struct TargetTriple {
    pub architecture: String,
    pub vendor: String,
    pub system: String,
    pub environment: String,
}

impl TargetTriple {
    pub fn new(architecture: String, vendor: String, system: String, environment: String) -> Self {
        Self {
            architecture,
            vendor,
            system,
            environment,
        }
    }

    pub fn to_string(&self) -> String {
        format!(
            "{}-{}-{}-{}",
            self.architecture, self.vendor, self.system, self.environment
        )
    }

    pub fn from_string(triple: String) -> Self {
        let parts: Vec<&str> = triple.split('-').collect();
        Self {
            architecture: parts[0].to_string(),
            vendor: parts[1].to_string(),
            system: parts[2].to_string(),
            environment: parts[3].to_string(),
        }
    }
}

/// Generic *(backend-agnostic)* options for the compiler. \
/// The options are used to configure the compiler's behavior
/// when compiling a module.
pub struct CompileOptions {
    pub optimization_level: u8,
    pub debug_info: bool,
    pub target: TargetTriple,
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
