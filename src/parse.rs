use nom::{branch::alt, bytes::complete::{tag, take_until}, character::complete::{alpha1, alphanumeric1, char, multispace0, one_of}, combinator::{map, opt, recognize, verify}, multi::{fold_many0, many0, many1}, sequence::{delimited, pair, preceded, separated_pair, terminated}};

use crate::{IResult, Span, analyse::{Located, Location}, error::ParseError, util::{ComplexNum, string_literal, ws, ws_tag}};

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
    Value(ComplexNum),
    Id(Located<String>),
    BinOp(BinOp, Box<(Located<Expr>, Located<Expr>)>),
    UnOp(UnOp, Box<Located<Expr>>),
    IfElse(Box<(Located<Expr>, Located<Expr>, Located<Expr>)>),
}

fn decimal(input: Span) -> IResult<Span> {
    recognize(many1(terminated(one_of("0123456789"), many0(tag("_")))))(input)
}

fn real(input: Span) -> IResult<Located<Expr>> {
    let left = Location::from(&input);
    let (input, integral) = decimal(input)?;
    let (input, rest) = opt(preceded(tag("."), decimal))(input)?;
    let right = Location::from(&input);

    let val = match rest {
        Some(rest) => integral.to_string() + &rest,
        None       => integral.to_string(),
    };

    let expr = val.parse::<f64>().map(|val| {
        Located::new(Expr::Value(ComplexNum(val, 0.)), left.span_to(right))
    }).map_err(|_| ParseError::error(input, "failed to parse decimal value".to_owned()))?;

    Ok((input, expr))
}

fn imag(input: Span) -> IResult<Located<Expr>> {
    let left = Location::from(&input);
    let (input, res) = terminated(recognize(opt(real)), tag("i"))(input)?;
    let right = Location::from(&input);
    
    let expr = if res.is_empty() {
        Ok(Expr::Value(ComplexNum(0., 1.)))
    } else {
        res.parse::<f64>().map(|val| Expr::Value(ComplexNum(0., val)))
    }.map_err(|_| ParseError::error(input, "failed to parse decimal value".to_owned()))?;

    Ok((input, Located::new(expr, left.span_to(right))))
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
    let left = Location::from(&input);
    let (input, id) = identifier(input)?;
    let right = Location::from(&input);
    let pos = left.span_to(right);
    let expr = Located::new(Expr::Id(Located::new(id.to_string(), pos)), pos);
    Ok((input, expr))
}

fn if_else(input: Span) -> IResult<Located<Expr>> {
    let left = Location::from(&input);
    let (input, ((cond, e_if), e_else)) = preceded(tag("if"),
    separated_pair(separated_pair(expression, tag("then"), expression),
        tag("else"),
        expression))(input)?;
    let right = Location::from(&input);
    let pos = left.span_to(right);

    let expr = Located::new(Expr::IfElse(Box::new((cond, e_if, e_else))), pos);
    Ok((input, expr))
}

fn conj(input: Span) -> IResult<Located<Expr>> {
    let (input, init) = basic_factor(input)?;
    let left = init.pos();

    let (input, elems) = many0(tag("^"))(input)?;

    let mut expr = init;
    for elem in elems {
        let right = Location::from(&elem);
        expr = Located::new(Expr::UnOp(UnOp::Conjugate, Box::new(expr)), left.span_to(right));
    }
    Ok((input, expr))
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
    let (input, result) = many0(preceded(ws_tag("**"), negate))(input)?;
    
    let mut iter = result.into_iter().rev();
    if let Some(mut expr) = iter.next() {
        for next in iter {
            let left = next.pos();
            let right = expr.pos();
            expr = Located::new(Expr::BinOp(BinOp::Power, Box::new((next, expr))), left.span_to(right));
        }
        let left = init.pos();
        let right = expr.pos();
        Ok((input, Located::new(Expr::BinOp(BinOp::Power, Box::new((init, expr))), left.span_to(right))))
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
    let pos = init.pos();

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
    let pos = init.pos();

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
    let pos = init.pos();

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
        Err(_) => Err(ParseError::fail(input, "expecting `;`".to_owned())),
    }
}

fn expect_open_brace(input: Span) -> IResult<()> {
    match ws_tag("{")(input) {
        Ok((input, _)) => Ok((input, ())),
        Err(_) => Err(ParseError::fail(input, "expecting `{`".to_owned())),
    }
}

fn expect_close_brace(input: Span) -> IResult<()> {
    match ws_tag("}")(input) {
        Ok((input, _)) => Ok((input, ())),
        Err(_) => Err(ParseError::fail(input, "expecting `}`".to_owned())),
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    PrintLit(String),
    Print(Located<Expr>),
    PrintLitLn(String),
    PrintLn(Located<Expr>),
    Let(Located<String>, Located<Expr>),
    LetMut(Located<String>, Located<Expr>),
    Assign(Located<String>, Located<Expr>),
    AddAssign(Located<String>, Located<Expr>),
    SubAssign(Located<String>, Located<Expr>),
    MulAssign(Located<String>, Located<Expr>),
    DivAssign(Located<String>, Located<Expr>),
    ModAssign(Located<String>, Located<Expr>),
    If(Located<Expr>, Vec<Located<Statement>>),
    IfElse(Located<Expr>, Vec<Located<Statement>>, Vec<Located<Statement>>),
    While(Located<Expr>, Vec<Located<Statement>>),
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
    let (input, _) = ws_tag("let")(input)?;
    let left = Location::from(&input);
    let (input, (id, expr)) = terminated(
        separated_pair(identifier, ws_tag("="), expression),
        ws_tag(";"))(input)?;
    let statement = Statement::Let(Located::new(id.to_string(), left.span_to(Location::from(&id))), expr);
    Ok((input, statement))
}

fn parse_let_mut(input: Span) -> IResult<Statement> {
    let (input, _) = preceded(ws_tag("let"), ws_tag("mut"))(input)?;
    let left = Location::from(&input);
    let (input, (id, expr)) = terminated(
        separated_pair(identifier, ws_tag("="), expression),
        ws_tag(";"))(input)?;
    let statement = Statement::LetMut(Located::new(id.to_string(), left.span_to(Location::from(&id))), expr);
    Ok((input, statement))
}

fn parse_assign(input: Span) -> IResult<Statement> {
    map(
        terminated(separated_pair(identifier, ws_tag("="), expression), expect_semicolon),
        |(id, expr)| Statement::Assign(Located::new(id.to_string(), Location::from(&id)), expr)
    )(input)
}

fn parse_add_assign(input: Span) -> IResult<Statement> {
    map(
        terminated(separated_pair(identifier, ws_tag("+="), expression), expect_semicolon),
        |(id, expr)| Statement::AddAssign(Located::new(id.to_string(), Location::from(&id)), expr)
    )(input)
}

fn parse_sub_assign(input: Span) -> IResult<Statement> {
    map(
        terminated(separated_pair(identifier, ws_tag("-="), expression), expect_semicolon),
        |(id, expr)| Statement::SubAssign(Located::new(id.to_string(), Location::from(&id)), expr)
    )(input)
}

fn parse_mul_assign(input: Span) -> IResult<Statement> {
    map(
        terminated(separated_pair(identifier, ws_tag("*="), expression), expect_semicolon),
        |(id, expr)| Statement::MulAssign(Located::new(id.to_string(), Location::from(&id)), expr)
    )(input)
}

fn parse_div_assign(input: Span) -> IResult<Statement> {
    map(
        terminated(separated_pair(identifier, ws_tag("/="), expression), expect_semicolon),
        |(id, expr)| Statement::DivAssign(Located::new(id.to_string(), Location::from(&id)), expr)
    )(input)
}

fn parse_mod_assign(input: Span) -> IResult<Statement> {
    map(
        terminated(separated_pair(identifier, ws_tag("%="), expression), expect_semicolon),
        |(id, expr)| Statement::ModAssign(Located::new(id.to_string(), Location::from(&id)), expr)
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

fn statement(input: Span) -> IResult<Located<Statement>> {
    // Throw away comments
    let (input, _) = opt(preceded(tag("--"), take_until("\n")))(input)?;
    let left = Location::from(&input);

    let (input, statement) = alt((parse_keyword,
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
        parse_mod_assign))(input)?;
    let right = Location::from(&input);

    Ok((input, Located::new(statement, left.span_to(right))))
}

/* ----------------------------------------------------------------
    FUNCTIONS
   ---------------------------------------------------------------- */

#[derive(Debug)]
pub struct Func {
    pub name: String,
    pub body: Vec<Located<Statement>>
}

pub fn parse_all(input: Span) -> IResult<Func> {
    let (input, _) = ws_tag("fn")(input)?;
    let (input, name) = map(identifier, |s| *s)(input)?;
    let (input, func) = map(
        preceded(preceded(ws_tag("()"), expect_open_brace),
            many0(statement)),
        move |body| Func { name: name.to_owned(), body }
    )(input)?;
    let (input, _) = expect_close_brace(input)?;
    Ok((input, func))
}
