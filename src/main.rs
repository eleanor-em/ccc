use std::{env, fs};

use ccomp::{Span, codegen, parse::parse_all};

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
    let path_index = filename.find('/').map(|x| x + 1).unwrap_or(0);
    let ext_index = filename.rfind('.').unwrap_or(filename.len());
    let raw_filename = &filename[path_index..ext_index];
    
    let (_remain, parsed) = parse_all(Span::new(&text)).unwrap();

    let ast_dest = format!("out/{}.ast", raw_filename);
    fs::create_dir_all("out")
        .expect("Failed to create `out` directory");
    fs::write(&ast_dest, format!("{:#?}", parsed))
        .expect(&format!("Failed to write AST output to {}", ast_dest));
    
    let llvm_dest = format!("out/{}.ll", raw_filename);
    match codegen::run(&llvm_dest, parsed) {
        Ok(_) => (),
        Err(e) => eprintln!("Failed to write LLVM IR output to {}: {}", llvm_dest, e),
    }
}
