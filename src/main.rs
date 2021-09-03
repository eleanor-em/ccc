use std::fs;

use ccomp::{Span, func::func};

const FILENAME: &str = "example/test.ccc";

fn main() {
    let text = fs::read_to_string(FILENAME)
        .expect("Could not read file");
    
    let (_remain, vals) = func(Span::new(&text)).unwrap();    
    println!("{:#?}", vals);
}
