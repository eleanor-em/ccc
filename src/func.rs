use nom::{combinator::map, multi::many0, sequence::{delimited, preceded}};

use crate::{IResult, Span, expr::identifier, statement::{Statement, statement}, ws_tag};

#[derive(Debug)]
pub struct Func(String, Vec<Statement>);

pub fn func(input: Span) -> IResult<Func> {
    let (input, _) = ws_tag("fn")(input)?;
    let (input, name) = map(identifier, |s| *s)(input)?;
    map(
        delimited(preceded(ws_tag("()"), ws_tag("{")), 
            many0(statement), 
            ws_tag("}")),
        move |body| Func(name.to_string(), body)
    )(input)
}
