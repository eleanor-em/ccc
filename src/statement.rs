use nom::{branch::alt, combinator::map, multi::many0, sequence::{delimited, preceded, separated_pair, terminated}};

use crate::{IResult, Span, expr::{Expr, expression, identifier}, ws_tag};

#[derive(Debug)]
pub enum Statement {
    Print(Expr),
    Let(String, Expr),
    Assign(String, Expr),
    If(Expr, Vec<Statement>),
    IfElse(Expr, Vec<Statement>, Vec<Statement>),
    While(Expr, Vec<Statement>),
}

fn parse_print(input: Span) -> IResult<Statement> {
    map(
        delimited(ws_tag("print"), expression, ws_tag(";")),
        Statement::Print
    )(input)
}

fn parse_let(input: Span) -> IResult<Statement> {
    map(
        delimited(ws_tag("let"), 
        separated_pair(identifier, ws_tag("="), expression),
        ws_tag(";")),
        |(id, expr)| Statement::Assign(id.to_string(), expr)
    )(input)
}

fn parse_assign(input: Span) -> IResult<Statement> {
    map(
        terminated(separated_pair(identifier, ws_tag("="), expression), ws_tag(";")),
        |(id, expr)| Statement::Assign(id.to_string(), expr)
    )(input)
}

fn parse_if(input: Span) -> IResult<Statement> {
    map(
        terminated(separated_pair(preceded(ws_tag("if"), expression), ws_tag("{"),
        many0(statement)),
        ws_tag("}")),
        |(cond, body)| Statement::If(cond, body)
    )(input)
}

fn parse_if_else(input: Span) -> IResult<Statement> {
    map(
        separated_pair(
            terminated(separated_pair(preceded(ws_tag("if"), expression), ws_tag("{"),
            many0(statement)),
            ws_tag("}")),
                ws_tag("else"),
                delimited(ws_tag("{"), many0(statement), ws_tag("}"))),
        |((cond, body_if), body_else)| Statement::IfElse(cond, body_if, body_else)
    )(input)
}

fn parse_while(input: Span) -> IResult<Statement> {
    map(
        terminated(separated_pair(preceded(ws_tag("while"), expression), ws_tag("{"),
        many0(statement)),
        ws_tag("}")),
        |(cond, body)| Statement::While(cond, body)
    )(input)
}

pub fn statement(input: Span) -> IResult<Statement> {
    alt((parse_print, parse_let, parse_while, parse_if_else, parse_if, parse_assign))(input)
}
