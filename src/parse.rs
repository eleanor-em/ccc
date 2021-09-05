use nom::{branch::alt, bytes::complete::{tag, take_until}, character::complete::{alpha1, alphanumeric1, char, multispace0, one_of}, combinator::{map, map_res, opt, recognize, verify}, multi::{fold_many0, many0, many1}, sequence::{delimited, pair, preceded, separated_pair, terminated}};

use crate::{ComplexInt, IResult, Span, util::string_literal, ws, ws_tag};

/* ----------------------------------------------------------------
    EXPRESSIONS
   ---------------------------------------------------------------- */

const RESERVED_WORDS: &[&str] = &[
    "if",
    "else",
    "then",
    "i",
    "let",
    "print",
    "println",
    "while",
    "fn",
    "mut",
    "break",
    "continue",
    // Below reserved for future use
    "matrix",
    "return",
    "pi",
    "tau",
];

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Plus,
    Minus,
    Times,
    Divide,
    Remainder,
    Power,
    Equals,
    NotEquals,
}

#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    Negate,
    Conjugate,
    Modulus,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Value(ComplexInt),
    Id(String),
    BinOp(BinOp, Box<(Expr, Expr)>),
    UnOp(UnOp, Box<Expr>),
    IfElse(Box<(Expr, Expr, Expr)>),
}

fn decimal(input: Span) -> IResult<Span> {
    recognize(many1(terminated(one_of("0123456789"), many0(tag("_")))))(input)
}

fn real(input: Span) -> IResult<Expr> {
    map_res(
        decimal,
        |s: Span| s.parse::<i64>().map(|val| Expr::Value(ComplexInt(val, 0)))
    )(input)
}

fn imag(input: Span) -> IResult<Expr> {
    map_res(
        terminated(recognize(opt(decimal)), tag("i")),
        |s: Span| {
            if s.is_empty() {
                Ok(Expr::Value(ComplexInt(0, 1)))
            } else {
                s.parse::<i64>().map(|val| Expr::Value(ComplexInt(0, val)))
            }
        }
    )(input)
}

fn value(input: Span) -> IResult<Expr> {
    alt((imag, real))(input)
}

pub fn identifier(input: Span) -> IResult<Span> {
    verify(recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_"), tag("'"))))
    )), |id: &Span| !RESERVED_WORDS.contains(id))(input)
}

fn identifier_expr(input: Span) -> IResult<Expr> {
    map(identifier, |id: Span| Expr::Id(id.to_string()))(input)
}

fn if_else(input: Span) -> IResult<Expr> {
    map(
        preceded(tag("if"),
                       separated_pair(separated_pair(expression, tag("then"), expression),
                       tag("else"),
                       expression)),
        |((cond, e_if), e_else)| Expr::IfElse(Box::new((cond, e_if, e_else))) 
    )(input)
}

fn negate(input: Span) -> IResult<Expr> {
    map(
        preceded(tag("-"), factor), 
        |e| Expr::UnOp(UnOp::Negate, Box::new(e))
    )(input)
}

fn conj(input: Span) -> IResult<Expr> {
    let (input, init) = basic_factor(input)?;

    fold_many0(
        tag("^"),
        move || init.clone(),
        |acc, _| {
            Expr::UnOp(UnOp::Conjugate, Box::new(acc))
        })(input)
}

fn modulus(input: Span) -> IResult<Expr> {
    map(
        delimited(tag("|"), expression, tag("|")), 
        |e| Expr::UnOp(UnOp::Modulus, Box::new(e))
    )(input)
}

fn parens(input: Span) -> IResult<Expr> {
    delimited(multispace0, 
        delimited(tag("("), expression, tag(")")), 
        multispace0)(input)
}

/// Basic factor, used to remove left recursion from conjugation i.e. A -> A^
fn basic_factor(input: Span) -> IResult<Expr> {
    alt((ws(identifier_expr),
         ws(if_else),
         ws(value),
         ws(modulus),
         ws(negate),
         parens))(input)
}

/// Either a basic factor or a conjugated basic factor
fn factor(input: Span) -> IResult<Expr> {
    alt((ws(conj), basic_factor))(input)
}

fn exp_factor(input: Span) -> IResult<Expr> {
    // Need to do a right fold, but nom doesn't easily support that, so implement it ourselves
    let (input, init) = factor(input)?;
    let (input, result) = many0(preceded(ws_tag("**"), factor))(input)?;
    let mut iter = result.into_iter().rev();
    if let Some(mut expr) = iter.next() {
        for next in iter {
            expr = Expr::BinOp(BinOp::Power, Box::new((next, expr)));
        }
        Ok((input, Expr::BinOp(BinOp::Power, Box::new((init, expr)))))
    } else {
        Ok((input, init))
    }
}

fn term(input: Span) -> IResult<Expr> {
    let (input, init) = exp_factor(input)?;

    fold_many0(
        pair(alt((char('*'), char('/'), char('%'))), exp_factor),
        move || init.clone(),
        |acc, (op, val): (char, Expr)| {
            let op = match op {
                '*' => BinOp::Times,
                '/' => BinOp::Divide,
                _   => BinOp::Remainder,
            };
            Expr::BinOp(op, Box::new((acc, val)))
        })(input)
}

fn expr(input: Span) -> IResult<Expr> {
    let (input, init) = term(input)?;

    fold_many0(
        pair(alt((char('+'), char('-'))), term),
        move || init.clone(),
        |acc, (op, val): (char, Expr)| {
                let op = match op {
                    '+' => BinOp::Plus,
                    _   => BinOp::Minus,
                };
                Expr::BinOp(op, Box::new((acc, val)))
        })(input)
}

fn equality(input: Span) -> IResult<Expr> {
    let (input, init) = expr(input)?;

    fold_many0(
        pair(alt((tag("=="), tag("!="))), expr),
        move || init.clone(),
        |acc, (op, val): (Span, Expr)| {
            let op = match *op {
                "==" => BinOp::Equals,
                _    => BinOp::NotEquals,
            };
            Expr::BinOp(op, Box::new((acc, val)))
        })(input)
}

pub fn expression(input: Span) -> IResult<Expr> {
    ws(equality)(input)
}
/* ----------------------------------------------------------------
    STATEMENTS
   ---------------------------------------------------------------- */

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

fn statement(input: Span) -> IResult<Statement> {
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

/* ----------------------------------------------------------------
    FUNCTIONS
   ---------------------------------------------------------------- */

#[derive(Debug)]
pub struct Func {
    pub name: String,
    pub body: Vec<Statement>
}

pub fn parse_all(input: Span) -> IResult<Func> {
    let (input, _) = ws_tag("fn")(input)?;
    let (input, name) = map(identifier, |s| *s)(input)?;
    map(
        delimited(preceded(ws_tag("()"), ws_tag("{")), 
            many0(statement), 
            ws_tag("}")),
        move |body| Func { name: name.to_string(), body }
    )(input)
}
