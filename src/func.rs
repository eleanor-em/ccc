use nom::{combinator::map, multi::many0, sequence::{delimited, preceded}};

use crate::{IResult, Span, expr::identifier, statement::{Statement, statement}, ws_tag};

#[derive(Debug)]
pub struct Func {
    pub name: String,
    pub body: Vec<Statement>
}

pub fn func(input: Span) -> IResult<Func> {
    let (input, _) = ws_tag("fn")(input)?;
    let (input, name) = map(identifier, |s| *s)(input)?;
    map(
        delimited(preceded(ws_tag("()"), ws_tag("{")), 
            many0(statement), 
            ws_tag("}")),
        move |body| Func { name: name.to_string(), body }
    )(input)
}
