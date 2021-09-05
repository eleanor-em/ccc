use std::{env, fs};

use ccomp::{Span, codegen, func::func};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("usage: ccc <filename>");
        return;
    }

    let text = fs::read_to_string(&args[1])
        .expect(&format!("Could not read file: {}", args[1]));
    
    let (_remain, parsed) = func(Span::new(&text)).unwrap();
    fs::write("out/test.ast", format!("{:#?}", parsed)).unwrap();
    codegen::run().unwrap();
}
