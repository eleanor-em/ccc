use std::{error::Error, fmt};

use inkwell::{execution_engine::FunctionLookupError, support::LLVMString};


#[derive(Debug)]
pub enum CompileError {
    InvalidState(&'static str),
    Llvm(LLVMString),
    Lookup(FunctionLookupError),
    NoMain,
    NotYetImplemented(String),
    UnknownSymbol(String),
    Unsupported(String),
    Immutable(String),
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // LLVMString escapes the newlines, unhelpfully.
            Self::Llvm(s) => write!(f, "LLVM error:\n{}", s.to_string().replace("\\n", "\n")),
            _ => write!(f, "{:?}", self),
        }
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

impl CompileError {
    pub fn not_yet_impl<T: fmt::Display>(meta: T) -> CompileError {
        Self::NotYetImplemented(format!("not yet implemented: {}", meta))
    }
    pub fn not_yet_impl_dbg<T: fmt::Debug>(meta: T) -> CompileError {
        Self::NotYetImplemented(format!("not yet implemented: {:?}", meta))
    }

    pub fn unknown_symbol(symbol: String) -> CompileError {
        Self::UnknownSymbol(format!("unknown symbol `{}`", symbol))
    }

    pub fn unsupported(meta: String) -> CompileError {
        Self::UnknownSymbol(format!("unsupported operation: {}", meta))
    }
}
