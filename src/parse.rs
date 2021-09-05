use nom::{branch::alt, bytes::complete::{tag, take_until}, character::complete::{alpha1, alphanumeric1, char, multispace0, one_of}, combinator::{map, map_res, opt, recognize, verify}, multi::{fold_many0, many0, many1}, sequence::{delimited, pair, preceded, separated_pair, terminated}};

use crate::{ComplexInt, IResult, Span, analyse::{Located, Location}, error::ParseError, util::string_literal, ws, ws_tag};

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
    BinOp(BinOp, Box<(Located<Expr>, Located<Expr>)>),
    UnOp(UnOp, Box<Located<Expr>>),
    IfElse(Box<(Located<Expr>, Located<Expr>, Located<Expr>)>),
}

fn decimal(input: Span) -> IResult<Span> {
    recognize(many1(terminated(one_of("0123456789"), many0(tag("_")))))(input)
}

fn real(input: Span) -> IResult<Located<Expr>> {
    let pos = Location::from(&input);
    map_res(
        decimal,
        move |s: Span| s.parse::<i64>().map(|val| {
            Located::new(Expr::Value(ComplexInt(val, 0)), pos)
        })
    )(input)
}

fn imag(input: Span) -> IResult<Located<Expr>> {
    let pos = Location::from(&input);
    let (input, res) = map_res(
        terminated(recognize(opt(decimal)), tag("i")),
        |s: Span| {
            if s.is_empty() {
                Ok(Expr::Value(ComplexInt(0, 1)))
            } else {
                s.parse::<i64>().map(|val| Expr::Value(ComplexInt(0, val)))
            }
        }
    )(input)?;
    Ok((input, Located::new(res, pos)))
}

fn value(input: Span) -> IResult<Located<Expr>> {
    alt((imag, real))(input)
}

pub fn identifier(input: Span) -> IResult<Span> {
    verify(recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_"), tag("'"))))
    )), |id: &Span| !RESERVED_WORDS.contains(id))(input)
}

fn identifier_expr(input: Span) -> IResult<Located<Expr>> {
    let pos = Location::from(&input);
    map(identifier, move |id: Span| Located::new(Expr::Id(id.to_string()), pos))(input)
}

fn if_else(input: Span) -> IResult<Located<Expr>> {
    let pos = Location::from(&input);
    map(
        preceded(tag("if"),
                       separated_pair(separated_pair(expression, tag("then"), expression),
                       tag("else"),
                       expression)),
        move |((cond, e_if), e_else)| {
            Located::new(Expr::IfElse(Box::new((cond, e_if, e_else))), pos)
        } 
    )(input)
}

fn conj(input: Span) -> IResult<Located<Expr>> {
    let (input, init) = basic_factor(input)?;
    let pos = init.at();

    fold_many0(
        tag("^"),
        move || init.clone(),
        move |acc, _| {
            Located::new(Expr::UnOp(UnOp::Conjugate, Box::new(acc)), pos)
        })(input)
}

fn modulus(input: Span) -> IResult<Located<Expr>> {
    let pos = Location::from(&input);
    map(
        delimited(tag("|"), expression, tag("|")), 
        move |e| Located::new(Expr::UnOp(UnOp::Modulus, Box::new(e)), pos)
    )(input)
}

fn parens(input: Span) -> IResult<Located<Expr>> {
    delimited(multispace0, 
        delimited(tag("("), expression, tag(")")), 
        multispace0)(input)
}

/// Basic factor, used to remove left recursion from conjugation i.e. A -> A^
fn basic_factor(input: Span) -> IResult<Located<Expr>> {
    alt((ws(identifier_expr),
         ws(if_else),
         ws(value),
         ws(modulus),
         parens))(input)
}

/// Either a basic factor or a conjugated basic factor
fn factor(input: Span) -> IResult<Located<Expr>> {
    alt((ws(conj), basic_factor))(input)
}

/// TODO: This can't parse 2**-2. Could maybe be fixed by adding another negation case?
fn exp_factor(input: Span) -> IResult<Located<Expr>> {
    // Need to do a right fold, but nom doesn't easily support that, so implement it ourselves
    let (input, init) = factor(input)?;
    let pos = init.at();

    let (input, result) = many0(preceded(ws_tag("**"), negate))(input)?;
    
    let mut iter = result.into_iter().rev();
    if let Some(mut expr) = iter.next() {
        for next in iter {
            expr = Located::new(Expr::BinOp(BinOp::Power, Box::new((next, expr))), pos);
        }
        Ok((input, Located::new(Expr::BinOp(BinOp::Power, Box::new((init, expr))), pos)))
    } else {
        Ok((input, init))
    }
}

fn negate(input: Span) -> IResult<Located<Expr>> {
    let pos = Location::from(&input);
    alt((map(
        preceded(tag("-"), exp_factor), 
        move |e| Located::new(Expr::UnOp(UnOp::Negate, Box::new(e)), pos)
    ), exp_factor))(input)
}

fn term(input: Span) -> IResult<Located<Expr>> {
    let mut fac = alt((ws(negate), exp_factor));
    let (input, init) = fac(input)?;
    let pos = init.at();

    fold_many0(
        pair(alt((char('*'), char('/'), char('%'))), fac),
        move || init.clone(),
        move |acc, (op, val): (char, Located<Expr>)| {
            let op = match op {
                '*' => BinOp::Times,
                '/' => BinOp::Divide,
                _   => BinOp::Remainder,
            };
            Located::new(Expr::BinOp(op, Box::new((acc, val))), pos)
        })(input)
}

fn expr(input: Span) -> IResult<Located<Expr>> {
    let (input, init) = term(input)?;
    let pos = init.at();

    fold_many0(
        pair(alt((char('+'), char('-'))), term),
        move || init.clone(),
        move |acc, (op, val): (char, Located<Expr>)| {
                let op = match op {
                    '+' => BinOp::Plus,
                    _   => BinOp::Minus,
                };
                Located::new(Expr::BinOp(op, Box::new((acc, val))), pos)
        })(input)
}

fn equality(input: Span) -> IResult<Located<Expr>> {
    let (input, init) = expr(input)?;
    let pos = init.at();

    fold_many0(
        pair(alt((tag("=="), tag("!="))), expr),
        move || init.clone(),
        move |acc, (op, val): (Span, Located<Expr>)| {
            let op = match *op {
                "==" => BinOp::Equals,
                _    => BinOp::NotEquals,
            };
            Located::new(Expr::BinOp(op, Box::new((acc, val))), pos)
        })(input)
}

pub fn expression(input: Span) -> IResult<Located<Expr>> {
    ws(equality)(input)
}
/* ----------------------------------------------------------------
    STATEMENTS
   ---------------------------------------------------------------- */

fn expect_semicolon(input: Span) -> IResult<()> {
    match ws_tag(";")(input) {
        Ok((input, _)) => Ok((input, ())),
        Err(_) => Err(nom::Err::Failure(ParseError::new(input, "expecting `;`".to_string()))),
    }
}

fn expect_open_brace(input: Span) -> IResult<()> {
    match ws_tag("{")(input) {
        Ok((input, _)) => Ok((input, ())),
        Err(_) => Err(nom::Err::Failure(ParseError::new(input, "expecting `{`".to_string()))),
    }
}

fn expect_close_brace(input: Span) -> IResult<()> {
    match ws_tag("}")(input) {
        Ok((input, _)) => Ok((input, ())),
        Err(_) => Err(nom::Err::Failure(ParseError::new(input, "expecting `}`".to_string()))),
    }
}

#[derive(Debug)]
pub enum Statement {
    PrintLit(String),
    Print(Located<Expr>),
    PrintLitLn(String),
    PrintLn(Located<Expr>),
    Let(String, Located<Expr>),
    LetMut(String, Located<Expr>),
    Assign(String, Located<Expr>),
    AddAssign(String, Located<Expr>),
    SubAssign(String, Located<Expr>),
    MulAssign(String, Located<Expr>),
    DivAssign(String, Located<Expr>),
    ModAssign(String, Located<Expr>),
    If(Located<Expr>, Vec<Statement>),
    IfElse(Located<Expr>, Vec<Statement>, Vec<Statement>),
    While(Located<Expr>, Vec<Statement>),
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
        delimited(ws_tag("println"), expression, expect_semicolon),
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
            expect_semicolon),
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
        terminated(separated_pair(identifier, ws_tag("="), expression), expect_semicolon),
        |(id, expr)| Statement::Assign(id.to_string(), expr)
    )(input)
}

fn parse_add_assign(input: Span) -> IResult<Statement> {
    map(
        terminated(separated_pair(identifier, ws_tag("+="), expression), expect_semicolon),
        |(id, expr)| Statement::AddAssign(id.to_string(), expr)
    )(input)
}

fn parse_sub_assign(input: Span) -> IResult<Statement> {
    map(
        terminated(separated_pair(identifier, ws_tag("-="), expression), expect_semicolon),
        |(id, expr)| Statement::SubAssign(id.to_string(), expr)
    )(input)
}

fn parse_mul_assign(input: Span) -> IResult<Statement> {
    map(
        terminated(separated_pair(identifier, ws_tag("*="), expression), expect_semicolon),
        |(id, expr)| Statement::MulAssign(id.to_string(), expr)
    )(input)
}

fn parse_div_assign(input: Span) -> IResult<Statement> {
    map(
        terminated(separated_pair(identifier, ws_tag("/="), expression), expect_semicolon),
        |(id, expr)| Statement::DivAssign(id.to_string(), expr)
    )(input)
}

fn parse_mod_assign(input: Span) -> IResult<Statement> {
    map(
        terminated(separated_pair(identifier, ws_tag("%="), expression), expect_semicolon),
        |(id, expr)| Statement::ModAssign(id.to_string(), expr)
    )(input)
}

fn parse_if(input: Span) -> IResult<Statement> {
    map(
        terminated(separated_pair(preceded(ws_tag("if"), expression), expect_open_brace,
        many0(statement)),
        expect_close_brace),
        |(cond, body)| Statement::If(cond, body)
    )(input)
}

fn parse_if_else(input: Span) -> IResult<Statement> {
    map(
        separated_pair(
            terminated(separated_pair(preceded(ws_tag("if"), expression), expect_open_brace,
            many0(statement)),
            expect_close_brace),
                ws_tag("else"),
                delimited(ws_tag("{"), many0(statement), ws_tag("}"))),
        |((cond, body_if), body_else)| Statement::IfElse(cond, body_if, body_else)
    )(input)
}

fn parse_while(input: Span) -> IResult<Statement> {
    map(
        terminated(separated_pair(preceded(ws_tag("while"), expression), expect_open_brace,
        many0(statement)),
        expect_close_brace),
        |(cond, body)| Statement::While(cond, body)
    )(input)
}

fn parse_keyword(input: Span) -> IResult<Statement> {
    alt((
        map(terminated(ws_tag("break"), expect_semicolon), |_| Statement::Break),
        map(terminated(ws_tag("continue"), expect_semicolon), |_| Statement::Continue),
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
    let (input, func) = map(
        preceded(preceded(ws_tag("()"), expect_open_brace),
            many0(statement)),
        move |body| Func { name: name.to_string(), body }
    )(input)?;
    let (input, _) = expect_close_brace(input)?;
    Ok((input, func))
}
