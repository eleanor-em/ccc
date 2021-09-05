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
        // Clippy: https://rust-lang.github.io/rust-clippy/master/index.html#expect_fun_call
        .unwrap_or_else(|_| panic!("Could not read file: {}", filename));

    // Figure out the "raw name" (without path or extension)
    let path_index = filename.find('/').map(|x| x + 1).unwrap_or(0);
    let ext_index = filename.rfind('.').unwrap_or(filename.len());
    let raw_filename = &filename[path_index..ext_index];

    let lines = text.as_str().split("\n").collect::<Vec<_>>();
    
    match parse_all(Span::new(&text)) {
        Ok((_remain, parsed)) =>  {
            let ast_dest = format!("out/{}.ast", raw_filename);
            fs::create_dir_all("out")
                .expect("Failed to create `out` directory");
            fs::write(&ast_dest, format!("{:#?}", parsed))
                .unwrap_or_else(|_| panic!("Failed to write AST output to {}", ast_dest));
            
            let llvm_dest = format!("out/{}.ll", raw_filename);
            match codegen::run(&llvm_dest, parsed) {
                Ok(_) => (),
                Err(e) => eprintln!("Failed to write LLVM IR output to {}: {}", llvm_dest, e),
            }
        },
        Err(e) => {
            match e {
                nom::Err::Incomplete(_) => panic!("Unexpected error while parsing (`Incomplete`)"),
                nom::Err::Error(e) | nom::Err::Failure(e) => {
                    // let line = e.span().fragment().split('\n').next().unwrap();
                    println!("error: at line {}, column {}:",
                        e.line(),
                        e.col());
                    if e.line() < lines.len() {
                        println!("\t{}\n\t{}^", lines[e.line() - 1], " ".repeat(e.col() - 1));
                    }
                    if let Some(msg) = e.msg() {
                        println!("{}", msg);
                    } else {
                        println!("unknown error");
                    }
                }
            }
        }
    } 
}
