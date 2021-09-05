use std::{error::Error, fmt};

use inkwell::{execution_engine::FunctionLookupError, support::LLVMString};


#[derive(Debug)]
pub enum CompileError {
    Llvm(LLVMString),
    Lookup(FunctionLookupError),
    NoMain,
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Error for CompileError {}

impl From<LLVMString> for CompileError {
    fn from(e: LLVMString) -> Self {
        Self::Llvm(e)
    }
}

impl From<FunctionLookupError> for CompileError {
    fn from(e: FunctionLookupError) -> Self {
        Self::Lookup(e)
    }
}
