use std::{error::Error, fmt};

use inkwell::{execution_engine::FunctionLookupError, support::LLVMString};

use crate::{Span, analyse::{Located, Location, SpanLength}};

/// From https://iximiuz.com/en/posts/rust-writing-parsers-with-nom/
#[derive(Debug, PartialEq)]
pub struct ParseError<'a> {
    span: Span<'a>,
    message: Option<String>,
}

impl<'a> ParseError<'a> {
    pub fn error(span: Span<'a>, message: String) -> nom::Err<Self> {
        nom::Err::Error(Self::new(span, message))
    }

    pub fn fail(span: Span<'a>, message: String) -> nom::Err<Self> {
        nom::Err::Failure(Self::new(span, message))
    }

    pub fn new(span: Span<'a>, message: String) -> Self {
        Self { span, message: Some(message) }
    }

    pub fn span(&self) -> &Span { &self.span }

    pub fn line(&self) -> usize { self.span().location_line() as usize }

    pub fn col(&self) -> usize { self.span().get_utf8_column() }

    pub fn msg(&self) -> Option<&str> {
        self.message.as_deref()
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
pub enum InternalError {
    InvalidState(&'static str),
    Llvm(LLVMString),
    Lookup(FunctionLookupError),
}

impl fmt::Display for InternalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // LLVMString escapes the newlines, unhelpfully.
            Self::Llvm(s) => write!(f, "LLVM error:\n{}", s.to_string().replace("\\n", "\n")),
            _ => write!(f, "{:?}", self),
        }
    }
}

impl Error for InternalError {}

impl InternalError {
    pub fn invalid_state(msg: &'static str) -> LocatedCompileError {
        LocatedCompileError::from_err(CompileError::Internal(Self::InvalidState(msg)))
    }

    pub fn llvm(llvm_str: LLVMString) -> LocatedCompileError {
        LocatedCompileError::from_err(CompileError::Internal(Self::Llvm(llvm_str)))
    }

    pub fn lookup(err: FunctionLookupError) -> LocatedCompileError {
        LocatedCompileError::from_err(CompileError::Internal(Self::Lookup(err)))
    }
}

#[derive(Debug)]
pub enum CompileError {
    Internal(InternalError),
    NoMain,
    NotYetImplemented(String),
    UnknownSymbol(String),
    Unsupported(String),
    Immutable(String),
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Internal(e) => write!(f, "{}", e),
            _ => write!(f, "{:?}", self),
        }
    }
}

impl Error for CompileError {}

#[derive(Debug)]
pub struct LocatedCompileError {
    pub pos: Option<Location>,
    err: CompileError,
    pub secondary_pos: Option<Location>,
    pub secondary_msg: Option<String>,
}

impl fmt::Display for LocatedCompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.err {
            CompileError::Internal(err) => {
                write!(f, "internal compiler error: {}", err)
            },
            CompileError::NoMain => {
                write!(f, "missing `main` function")
            },
            CompileError::NotYetImplemented(msg)
                | CompileError::UnknownSymbol(msg)
                | CompileError::Unsupported(msg)
                | CompileError::Immutable(msg) => {
                write!(f, "{}", msg)
            },
        }
    }
}

impl From<LLVMString> for LocatedCompileError {
    fn from(e: LLVMString) -> Self {
        InternalError::llvm(e)
    }
}

impl From<FunctionLookupError> for LocatedCompileError {
    fn from(e: FunctionLookupError) -> Self {
        InternalError::lookup(e)
    }
}

impl LocatedCompileError {
    fn from_err(err: CompileError) -> Self{
        Self { pos: None, err, secondary_msg: None, secondary_pos: None }
    }

    fn new(pos: Location, err: CompileError) -> Self {
        Self { pos: Some(pos), err, secondary_msg: None, secondary_pos: None }
    }

    fn with_secondary(pos: Location, err: CompileError, secondary_msg: String, secondary_pos: Location) -> Self {
        Self { pos: Some(pos), err, secondary_msg: Some(secondary_msg), secondary_pos: Some(secondary_pos) }
    }

    pub fn not_yet_impl<T: fmt::Display>(pos: Location, meta: T) -> LocatedCompileError {
        Self::new(pos, CompileError::NotYetImplemented(format!("not yet implemented: {}", meta)))
    }
    pub fn not_yet_impl_dbg<T: fmt::Debug>(pos: Location, meta: T) -> LocatedCompileError {
        Self::new(pos, CompileError::NotYetImplemented(format!("not yet implemented: {:?}", meta)))
    }

    pub fn unsupported(pos: Location, meta: String) -> LocatedCompileError {
        Self::new(pos, CompileError::Unsupported(format!("unsupported operation: {}", meta)))
    }

    pub fn immutable(statement_pos: Location, id: String, decl: Location) -> LocatedCompileError {
        Self::with_secondary(statement_pos,
            CompileError::Immutable(format!("attempted to mutate `{}`", id)),
            format!("`{}` declared immutable here:", id),
            decl)
    }

    pub fn unknown_symbol(id: Located<String>) -> LocatedCompileError {
        Self::new(id.pos(), CompileError::UnknownSymbol(format!("unknown symbol: `{}`", id.val())))
    }

    pub fn no_main() -> LocatedCompileError {
        Self::new(Location { line: 0, col: 0, len: SpanLength::None }, CompileError::NoMain)
    }
}
