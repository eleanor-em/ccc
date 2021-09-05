use nom::{branch::alt, bytes::complete::{tag, take_until}, combinator::{map, opt}, multi::many0, sequence::{delimited, preceded, separated_pair, terminated}};

use crate::{IResult, Span, expr::{Expr, expression, identifier}, util::string_literal, ws_tag};

#[derive(Debug)]
pub enum Statement {
    Print(Expr),
    PrintLit(String),
    PrintLn(Expr),
    PrintLitLn(String),
    Let(String, Expr),
    LetMut(String, Expr),
    Assign(String, Expr),
    AddAssign(String, Expr),
    SubAssign(String, Expr),
    MulAssign(String, Expr),
    DivAssign(String, Expr),
    ModAssign(String, Expr),
    If(Expr, Vec<Statement>),
    IfElse(Expr, Vec<Statement>, Vec<Statement>),
    While(Expr, Vec<Statement>),
    Break,
    Continue,
}

fn parse_print(input: Span) -> IResult<Statement> {
    map(
        delimited(ws_tag("print"), expression, ws_tag(";")),
        Statement::Print
    )(input)
}

fn parse_print_lit(input: Span) -> IResult<Statement> {
    map(
        delimited(ws_tag("print"), string_literal, ws_tag(";")),
        Statement::PrintLit
    )(input)
}

fn parse_print_ln(input: Span) -> IResult<Statement> {
    map(
        delimited(ws_tag("println"), expression, ws_tag(";")),
        Statement::PrintLn
    )(input)
}

fn parse_print_lit_ln(input: Span) -> IResult<Statement> {
    map(
        delimited(ws_tag("println"), string_literal, ws_tag(";")),
        Statement::PrintLitLn
    )(input)
}

fn parse_let(input: Span) -> IResult<Statement> {
    map(
        delimited(ws_tag("let"), 
            separated_pair(identifier, ws_tag("="), expression),
            ws_tag(";")),
        |(id, expr)| Statement::Let(id.to_string(), expr)
    )(input)
}

fn parse_let_mut(input: Span) -> IResult<Statement> {
    map(
        delimited(preceded(ws_tag("let"), ws_tag("mut")),
            separated_pair(identifier, ws_tag("="), expression),
            ws_tag(";")),
        |(id, expr)| Statement::LetMut(id.to_string(), expr)
    )(input)
}

fn parse_assign(input: Span) -> IResult<Statement> {
    map(
        terminated(separated_pair(identifier, ws_tag("="), expression), ws_tag(";")),
        |(id, expr)| Statement::Assign(id.to_string(), expr)
    )(input)
}

fn parse_add_assign(input: Span) -> IResult<Statement> {
    map(
        terminated(separated_pair(identifier, ws_tag("+="), expression), ws_tag(";")),
        |(id, expr)| Statement::AddAssign(id.to_string(), expr)
    )(input)
}

fn parse_sub_assign(input: Span) -> IResult<Statement> {
    map(
        terminated(separated_pair(identifier, ws_tag("-="), expression), ws_tag(";")),
        |(id, expr)| Statement::SubAssign(id.to_string(), expr)
    )(input)
}

fn parse_mul_assign(input: Span) -> IResult<Statement> {
    map(
        terminated(separated_pair(identifier, ws_tag("*="), expression), ws_tag(";")),
        |(id, expr)| Statement::MulAssign(id.to_string(), expr)
    )(input)
}

fn parse_div_assign(input: Span) -> IResult<Statement> {
    map(
        terminated(separated_pair(identifier, ws_tag("/="), expression), ws_tag(";")),
        |(id, expr)| Statement::DivAssign(id.to_string(), expr)
    )(input)
}

fn parse_mod_assign(input: Span) -> IResult<Statement> {
    map(
        terminated(separated_pair(identifier, ws_tag("%="), expression), ws_tag(";")),
        |(id, expr)| Statement::ModAssign(id.to_string(), expr)
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

fn parse_keyword(input: Span) -> IResult<Statement> {
    alt((
        map(terminated(ws_tag("break"), ws_tag(";")), |_| Statement::Break),
        map(terminated(ws_tag("continue"), ws_tag(";")), |_| Statement::Continue),
    ))(input)
}

pub fn statement(input: Span) -> IResult<Statement> {
    // Throw away comments
    let (input, _) = opt(preceded(tag("--"), take_until("\n")))(input)?;

    alt((parse_keyword,
        parse_print_lit_ln,
        parse_print_ln,
        parse_print_lit,
        parse_print,
        parse_let_mut,
        parse_let,
        parse_while,
        parse_if_else,
        parse_if,
        parse_assign,
        parse_add_assign,
        parse_sub_assign,
        parse_mul_assign,
        parse_div_assign,
        parse_mod_assign))(input)
}
