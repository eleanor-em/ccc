use nom::{branch::alt, bytes::complete::tag, character::complete::{alpha1, alphanumeric1, char, multispace0, one_of}, combinator::{map, map_res, opt, recognize, verify}, multi::{fold_many0, many0, many1}, sequence::{delimited, pair, preceded, separated_pair, terminated}};

use crate::{Complex, IResult, Span, ws};

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

#[derive(Debug, Clone)]
pub enum Expr {
    Value(Complex),
    Id(String),
    Modulus(Box<Expr>),
    Plus(Box<(Expr, Expr)>),
    Times(Box<(Expr, Expr)>),
    Minus(Box<(Expr, Expr)>),
    Divide(Box<(Expr, Expr)>),
    Remainder(Box<(Expr, Expr)>),
    Negate(Box<Expr>),
    Conjugate(Box<Expr>),
    IfElse(Box<(Expr, Expr, Expr)>),
    Equals(Box<(Expr, Expr)>),
    NotEquals(Box<(Expr, Expr)>),
}

fn decimal(input: Span) -> IResult<Span> {
    recognize(many1(terminated(one_of("0123456789"), many0(tag("_")))))(input)
}

fn real(input: Span) -> IResult<Expr> {
    map_res(
        decimal,
        |s: Span| s.parse::<i64>().map(|val| Expr::Value(Complex(val, 0)))
    )(input)
}

fn imag(input: Span) -> IResult<Expr> {
    map_res(
        terminated(recognize(opt(decimal)), tag("i")),
        |s: Span| {
            if s.is_empty() {
                Ok(Expr::Value(Complex(0, 1)))
            } else {
                s.parse::<i64>().map(|val| Expr::Value(Complex(0, val)))
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
        |e| Expr::Negate(Box::new(e))
    )(input)
}

fn conj(input: Span) -> IResult<Expr> {
    let (input, init) = basic_factor(input)?;

    fold_many0(
        tag("^"),
        move || init.clone(),
        |acc, _| {
            Expr::Conjugate(Box::new(acc))
        })(input)
}

fn modulus(input: Span) -> IResult<Expr> {
    map(
        delimited(tag("|"), expression, tag("|")), 
        |e| Expr::Modulus(Box::new(e))
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

fn term(input: Span) -> IResult<Expr> {
    let (input, init) = factor(input)?;

    fold_many0(
        pair(alt((char('*'), char('/'), char('%'))), factor),
        move || init.clone(),
        |acc, (op, val): (char, Expr)| {
            match op {
                '*' => Expr::Times(Box::new((acc, val))),
                '/' => Expr::Divide(Box::new((acc, val))),
                _   => Expr::Remainder(Box::new((acc, val))),
            }
        })(input)
}

fn expr(input: Span) -> IResult<Expr> {
    let (input, init) = term(input)?;

    fold_many0(
        pair(alt((char('+'), char('-'))), term),
        move || init.clone(),
        |acc, (op, val): (char, Expr)| {
            match op {
                '+' => Expr::Plus(Box::new((acc, val))),
                _   => Expr::Minus(Box::new((acc, val))),
            }
        })(input)
}

fn equality(input: Span) -> IResult<Expr> {
    let (input, init) = expr(input)?;

    fold_many0(
        pair(alt((tag("=="), tag("!="))), expr),
        move || init.clone(),
        |acc, (op, val): (Span, Expr)| {
            match *op {
                "==" => Expr::Equals(Box::new((acc, val))),
                _    => Expr::NotEquals(Box::new((acc, val))),
            }
        })(input)
}

pub fn expression(input: Span) -> IResult<Expr> {
    ws(equality)(input)
}
