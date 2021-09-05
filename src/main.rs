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

    // Store a copy of the source code to make error reporting easier down the line
    let lines = text.as_str().split('\n').collect::<Vec<_>>();
    
    match parse_all(Span::new(&text)) {
        // In this case, we can simply export the LLVM and run the code in a JIT environment
        Ok((_remain, parsed)) =>  {
            let ast_dest = format!("out/{}.ast", raw_filename);
            fs::create_dir_all("out")
                .expect("Failed to create `out` directory");
            fs::write(&ast_dest, format!("{:#?}", parsed))
                .unwrap_or_else(|_| panic!("Failed to write AST output to {}", ast_dest));
            
            let llvm_dest = format!("out/{}.ll", raw_filename);
            // Check if we had a compiler error, and attempt to explain it
            if let Err(e) = codegen::run(&llvm_dest, parsed) {
                print!("\u{001b}[31;1merror\u{001b}[37;1m: {}", e);
                if let Some(pos) = e.pos {
                    print!(" at line {}, column {}:", pos.line, pos.col);
                    if pos.line < lines.len() {
                        let line = lines[pos.line - 1];
                        let trimmed = line.trim_start();
                        let begin_whitespace = line.len() - trimmed.len();
                        print!("\n\u{001b}[0m {:4} |\t\t{}\n       \t\t{}\u{001b}[31;1m^\u{001b}[0m", pos.line, trimmed, " ".repeat(pos.col - 1 - begin_whitespace));
                    }
                } else {
                    print!(":\u{001b}[0m");
                }
                if let Some(msg) = e.secondary_msg {
                    print!("\n\u{001b}[36;1mnote\u{001b}[37;1m: {}", msg);
                }
                if let Some(pos) = e.secondary_pos {
                    if pos.line < lines.len() {
                        let line = lines[pos.line - 1];
                        let trimmed = line.trim_start();
                        let begin_whitespace = line.len() - trimmed.len();
                        print!("\u{001b}[0m\n {:4} |\t\t{}\n       \t\t{}\u{001b}[36;1m^\u{001b}[0m", pos.line, trimmed, " ".repeat(pos.col - 1 - begin_whitespace));
                    }
                }
                print!("\u{001b}[0m\n\n");
            }
        },
        // Otherwise, we hit a parse error, so try to report that
        Err(e) => {
            match e {
                nom::Err::Incomplete(_) => panic!("Unexpected error while parsing (`Incomplete`)"),
                nom::Err::Error(e) | nom::Err::Failure(e) => {
                    println!("\u{001b}[31;1merror\u{001b}[37;1m: at line {}, column {}:\u{001b}[0m",
                        e.line(),
                        e.col());
                    if e.line() < lines.len() {
                        println!("\t{}\n\t{}\u{001b}[31;1m^\u{001b}[0m", lines[e.line() - 1], " ".repeat(e.col() - 1));
                    }
                    print!("\u{001b}[37;1m");
                    if let Some(msg) = e.msg() {
                        println!("{}", msg);
                    } else {
                        println!("unknown error");
                    }
                    print!("\u{001b}[0m");
                }
            }
        }
    } 
}
