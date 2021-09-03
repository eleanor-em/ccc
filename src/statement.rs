use nom::{branch::alt, combinator::map, sequence::{preceded, separated_pair, terminated}};

use crate::{IResult, Span, expr::{Expr, expression, identifier}, ws_tag};

#[derive(Debug)]
pub enum Statement {
    Print(Expr),
    Let(String, Expr),
    Assign(String, Expr),
}

fn parse_print(input: Span) -> IResult<Statement> {
    map(
        preceded(ws_tag("print"), expression),
        Statement::Print
    )(input)    
}

fn parse_let(input: Span) -> IResult<Statement> {
    map(
        preceded(ws_tag("let"), separated_pair(identifier, ws_tag("="), expression)),
        |(id, expr)| Statement::Assign(id.to_string(), expr)
    )(input)
}

fn parse_assign(input: Span) -> IResult<Statement> {
    map(
        separated_pair(identifier, ws_tag("="), expression),
        |(id, expr)| Statement::Assign(id.to_string(), expr)
    )(input)
}

pub fn statement(input: Span) -> IResult<Statement> {
    terminated(alt((parse_print, parse_let, parse_assign)), ws_tag(";"))(input)
}
