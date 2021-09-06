use nom_locate::LocatedSpan;
use crate::error::ParseError;

pub mod analyse;
pub mod builtins;
pub mod codegen;
pub mod error;
pub mod parse;
pub mod util;

pub type Span<'a> = LocatedSpan<&'a str>;
pub type IResult<'a, O> = nom::IResult<Span<'a>, O, ParseError<'a>>;
