use nom::{bytes::complete::tag, character::complete::multispace0, sequence::delimited};
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

#[derive(Debug, Clone, Copy)]
pub struct ComplexInt(i64, i64);

pub fn ws<'a, F: 'a, O>(f: F) -> impl FnMut(Span<'a>) -> IResult<O>
        where
            F: Fn(Span) -> IResult<O> {
    delimited(multispace0, f, multispace0)
}


pub fn ws_tag<'a>(s: &'static str) -> impl FnMut(Span<'a>) -> IResult<Span<'a>> {
    delimited(multispace0, tag(s), multispace0)
}
