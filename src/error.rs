use std::{error::Error, fmt};

use inkwell::{execution_engine::FunctionLookupError, support::LLVMString};

use crate::Span;

/// From https://iximiuz.com/en/posts/rust-writing-parsers-with-nom/
#[derive(Debug, PartialEq)]
pub struct ParseError<'a> {
    span: Span<'a>,
    message: Option<String>,
}

impl<'a> ParseError<'a> {
    pub fn new(span: Span<'a>, message: String) -> Self {
        Self { span, message: Some(message) }
    }

    pub fn span(&self) -> &Span { &self.span }

    pub fn line(&self) -> usize { self.span().location_line() as usize }

    pub fn col(&self) -> usize { self.span().get_utf8_column() }

    pub fn msg(&self) -> Option<&str> {
        self.message.as_ref().map(|s| s.as_str())
    }
}

impl<'a> nom::error::ParseError<Span<'a>> for ParseError<'a> {
    fn from_error_kind(input: Span<'a>, kind: nom::error::ErrorKind) -> Self {
        Self::new(input, format!("parse error: {:?}", kind))
    }

    fn append(_input: Span<'a>, _kind: nom::error::ErrorKind, other: Self) -> Self {
        other
    }

    fn from_char(input: Span<'a>, c: char) -> Self {
        Self::new(input, format!("unexpected character '{}'", c))
    }
}

impl<'a> From<nom::Err<nom::error::Error<&str>>> for ParseError<'a> {
    fn from(_: nom::Err<nom::error::Error<&str>>) -> Self {
        todo!()
    }
}

impl<'a> From<nom::Err<nom::error::Error<Span<'a>>>> for ParseError<'a> {
    fn from(_: nom::Err<nom::error::Error<Span<'a>>>) -> Self {
        todo!()
    }
}

impl<'a, E: Error> nom::error::FromExternalError<Span<'a>, E> for ParseError<'a> {
    fn from_external_error(input: Span<'a>, _: nom::error::ErrorKind, e: E) -> Self {
        Self::new(input, format!("external error: {:?}", e))
    }
}

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
