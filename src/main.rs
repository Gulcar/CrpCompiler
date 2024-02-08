use std::{env, fs::File};
use std::io::{self, Write};
use std::process::Command;

mod lexer;
mod ast;
mod generator;

use ast::ASTNode;

fn print_usage_exit(exe_name: &str) -> ! {
    eprintln!("usage: {} <file> [options..]", exe_name);
    eprintln!("options:");
    eprintln!("  -v --verbose");
    std::process::exit(1)
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        print_usage_exit(&args[0]);
    }

    let source_file = &args[1];
    let asm_file = "a.asm";
    let output_file = "a.out";

    for arg in &args[2..] {
        match arg.as_str() {
            "-v" | "--verbose" => {
                println!("pozdravljen svet!");
            }
            _ => {
                eprintln!("ERROR: unknown argument: {}", arg);
                std::process::exit(1);
            }
        }
    }

    let tokens = match lexer::lex(source_file) {
        Ok(tokens) => tokens,
        Err(_) => {
            println!("compilation failed (lexer error)");
            std::process::exit(1)
        }
    };
    dbg!(&tokens);

    let ast = ASTNode::parse_root(&tokens);
    dbg!(&ast);

    let mut file = match File::create(asm_file) {
        Ok(f) => f,
        Err(e) => {
            println!("failed to open file: {} ({})", asm_file, e);
            std::process::exit(1)
        },
    };

    generator::write_asm(&ast, &mut file).unwrap();
    generator::write_asm(&ast, &mut io::stdout()).unwrap();

    file.flush().unwrap();

    let nasm_output = Command::new("nasm")
        .arg("-felf64")
        .arg(asm_file)
        .output().unwrap();

    if nasm_output.status.success() == false {
        dbg!(nasm_output);
        println!("ERROR: nasm failed");
        std::process::exit(1);
    }

    let gcc_output = Command::new("gcc")
        .arg("a.o")
        .output().unwrap();

    if gcc_output.status.success() == false {
        dbg!(gcc_output);
        println!("ERROR: gcc failed");
        std::process::exit(1);
    }
}
