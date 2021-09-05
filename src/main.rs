use std::{env, fs};

use ccomp::{Span, codegen, func::func};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("usage: ccc <filename>");
        return;
    }

    let filename = args[1].as_str();

    let text = fs::read_to_string(filename)
        .expect(&format!("Could not read file: {}", filename));

    // Figure out the "raw name" (without path or extension)
    let path_index = filename.find('/').unwrap_or(0);
    let ext_index = filename.rfind('.').unwrap_or(filename.len());
    let raw_filename = &filename[path_index..ext_index];
    
    let (_remain, parsed) = func(Span::new(&text)).unwrap();

    fs::create_dir_all("out")
        .expect("Failed to create `out` directory");
    fs::write(format!("out/{}.ast", raw_filename), format!("{:#?}", parsed))
        .expect(&format!("Failed to write AST output to out/{}.ast", raw_filename));
    codegen::run(&format!("out/{}.ll", raw_filename))
        .expect(&format!("Failed to write LLVM IR output to out/{}.ll", raw_filename));
}
