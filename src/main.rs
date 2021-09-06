use std::{env, fs};

use ccomp::{Span, analyse::SpanLength, codegen, parse::parse_all};

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

    // Terminal colours
    const TERM_RED: &str = "\u{001b}[31;1m";
    const TERM_RESET: &str = "\u{001b}[0m";
    const TERM_WHITE: &str = "\u{001b}[37;1m";
    const TERM_BLUE: &str = "\u{001b}[36;1m";
    
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
                print!("{}error{}: {}:", TERM_RED, TERM_WHITE, e);
                // Show the position if available
                if let Some(pos) = e.pos {
                    print!("\n{}{}:{}:{}", TERM_RESET, filename, pos.line, pos.col);
                    // If possible, show the offending line of source
                    if pos.line < lines.len() {
                        let line = lines[pos.line - 1];
                        let trimmed = line.trim_start();
                        let begin_whitespace = line.len() - trimmed.len();
                        let space_count = pos.col - 1 - begin_whitespace;
                        let underline_count = match pos.len {
                            SpanLength::Size(len) => len,
                            SpanLength::ToEnd => {
                                // FIXME: this is hack to deal with semicolon underlining
                                trimmed.len() - space_count -
                                    if line.chars().last().unwrap() == ';' {
                                        1
                                    } else {
                                        0
                                    }
                                }
                            SpanLength::None  => 1,
                        };
                        let underline = if underline_count > 1 {
                            "└".to_owned() + &"─".repeat(underline_count - 2) + "┘"
                        } else {
                            "^".to_owned()
                        };
                        print!("\n{:3} |\t{}\n     \t{}{}{}{}",
                               pos.line,
                               trimmed,
                               " ".repeat(space_count),
                               TERM_RED,
                               underline,
                               TERM_RESET);
                    }
                }
                // Show the note if one exists
                if let Some(msg) = e.secondary_msg {
                    print!("\n{}note{}: {}", TERM_BLUE, TERM_WHITE, msg);
                }
                // If the note points to a location, show the location and line of code
                if let Some(pos) = e.secondary_pos {
                    if pos.line < lines.len() {
                        let line = lines[pos.line - 1];
                        let trimmed = line.trim_start();
                        let begin_whitespace = line.len() - trimmed.len();
                        let space_count = pos.col - 1 - begin_whitespace;
                        let underline_count = match pos.len {
                            SpanLength::Size(len) => len,
                            SpanLength::ToEnd => {
                                // FIXME: this is hack to deal with semicolon underlining
                                trimmed.len() - space_count -
                                    if line.chars().last().unwrap() == ';' {
                                        1
                                    } else {
                                        0
                                    }
                                }
                            SpanLength::None  => 1,
                        };
                        let underline = if underline_count > 1 {
                            "└".to_owned() + &"─".repeat(underline_count - 2) + "┘"
                        } else {
                            "^".to_owned()
                        };
                        print!("\n{}{:3} |\t{}\n    \t{}{}{}{}",
                               TERM_RESET,
                               pos.line,
                               trimmed,
                               " ".repeat(space_count),
                               TERM_BLUE,
                               underline,
                               TERM_RESET);
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
                    println!("{}error{}: at line {}, column {}:{}",
                        TERM_RED,
                        TERM_WHITE,
                        e.line(),
                        e.col(),
                        TERM_RESET);
                    if e.line() < lines.len() {
                        println!("\t{}\n\t{}{}^",
                                 lines[e.line() - 1],
                                 " ".repeat(e.col() - 1),
                                 TERM_RED);
                    }
                    print!("{}", TERM_WHITE);
                    if let Some(msg) = e.msg() {
                        println!("{}", msg);
                    } else {
                        println!("unknown error");
                    }
                    print!("{}", TERM_RESET);
                }
            }
        }
    } 
}
