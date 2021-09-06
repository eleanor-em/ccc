/// Taken from https://raw.githubusercontent.com/Geal/nom/master/examples/string.rs

use nom::{branch::alt, bytes::complete::tag, character::complete::multispace0};
use nom::bytes::streaming::{is_not, take_while_m_n};
use nom::character::streaming::{char, multispace1};
use nom::combinator::{map, map_opt, value, verify};
use nom::multi::fold_many0;
use nom::sequence::{delimited, preceded};
use nom::IResult;

use crate::{Span, error::ParseError};

fn parse_unicode(input: &str) -> IResult<&str, char> {
    let parse_hex = take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit());
    let parse_delimited_hex = preceded(
        char('u'),
        delimited(char('{'), parse_hex, char('}')),
    );

    let parse_u32 = map(
        parse_delimited_hex,
        move |hex| u32::from_str_radix(hex, 16).unwrap()
    );

    map_opt(parse_u32, std::char::from_u32)(input)
}

/// Parse an escaped character: \n, \t, \r, \u{00AC}, etc.
fn parse_escaped_char(input: &str) -> IResult<&str, char> {
    preceded(
        char('\\'),
        alt((
            parse_unicode,
            value('\n', char('n')),
            value('\r', char('r')),
            value('\t', char('t')),
            value('\u{08}', char('b')),
            value('\u{0C}', char('f')),
            value('\\', char('\\')),
            value('/', char('/')),
            value('"', char('"')),
        )),
    )(input)
}

/// Parse a backslash, followed by any amount of whitespace. This is used later
/// to discard any escaped whitespace.
fn parse_escaped_whitespace(input: &str) -> IResult<&str, &str> {
    preceded(char('\\'), multispace1)(input)
}

/// Parse a non-empty block of text that doesn't include \ or "
fn parse_literal(input: &str) -> IResult<&str, &str> {
    let not_quote_slash = is_not("\"\\");
    verify(not_quote_slash, |s: &str| !s.is_empty())(input)
}

/// A string fragment contains a fragment of a string being parsed: either
/// a non-empty Literal (a series of non-escaped characters), a single
/// parsed escaped character, or a block of escaped whitespace.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StringFragment<'a> {
    Literal(&'a str),
    EscapedChar(char),
    EscapedWS,
}

/// Combine parse_literal, parse_escaped_whitespace, and parse_escaped_char
/// into a StringFragment.
fn parse_fragment(input: &str) -> IResult<&str, StringFragment> {
    alt((
        map(parse_literal, StringFragment::Literal),
        map(parse_escaped_char, StringFragment::EscapedChar),
        value(StringFragment::EscapedWS, parse_escaped_whitespace),
    ))(input)
}

/// Parse a string. Use a loop of parse_fragment and push all of the fragments
/// into an output string.
fn parse_string(input: &str) -> IResult<&str, String> {
    let build_string = fold_many0(
        parse_fragment,
        String::new,
        |mut string, fragment| {
            match fragment {
                StringFragment::Literal(s) => string.push_str(s),
                StringFragment::EscapedChar(c) => string.push(c),
                StringFragment::EscapedWS => {}
            }
            string
        }
    );

    delimited(char('"'), build_string, char('"'))(input)
}

pub fn string_literal(input: Span) -> crate::IResult<String> {
    match parse_string(&input) {
        Ok((input, lit)) => Ok((Span::new(input), lit)),
        Err(_) => Err(nom::Err::Error(ParseError::new(input, "invalid string literal".to_owned())))
    }
}


#[derive(Debug, Clone, Copy)]
pub struct ComplexInt(pub i64, pub i64);

pub fn ws<'a, F: 'a, O>(f: F) -> impl FnMut(Span<'a>) -> crate::IResult<O>
        where
            F: Fn(Span) -> crate::IResult<O> {
    delimited(multispace0, f, multispace0)
}


pub fn ws_tag<'a>(s: &'static str) -> impl FnMut(Span<'a>) -> crate::IResult<Span<'a>> {
    delimited(multispace0, tag(s), multispace0)
}
