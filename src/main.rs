use std::fs;

use ccomp::{Span, statement::statement};
use nom::multi::many0;

const FILENAME: &str = "example/test.ccc";

fn main() {
    let text = fs::read_to_string(FILENAME)
        .expect("Could not read file");
    
    let (remain, vals) = many0(statement)(Span::new(&text)).unwrap();
    if remain.fragment().trim().len() > 0 {
        let line = remain.location_line();
        let next_line = remain.fragment().split('\n').next().unwrap_or("");
        let next = if next_line.is_empty() {
            "".to_string()
        } else {
            String::from(next_line.as_bytes()[0] as char)
        };

        let orig_line = text.split('\n').collect::<Vec<&str>>()[(line - 1) as usize];
        println!("error: at line {}, column {}: unexpected symbol `{}`:\n\t{}\n\t{}^",
                 line,
                 remain.get_column(),
                 next,
                 orig_line,
                 " ".repeat(remain.get_column() - 1));
    } else {
        println!("{:#?}", vals);
    }
}
