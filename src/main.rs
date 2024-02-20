use ast::ASTProgram;
use generator::ASMGenerator;
use std::io::{self, Write};
use std::process::Command;
use std::{env, fs::File};

use crate::validation::validate;

mod ast;
mod generator;
mod lexer;
mod validation;

// https://norasandler.com/2017/11/29/Write-a-Compiler.html

fn print_usage_exit(exe_name: &str) -> ! {
    eprintln!("usage: {} <file> [options..]", exe_name);
    eprintln!("options:");
    eprintln!("  -h --help");
    eprintln!("  -o --output <file>");
    std::process::exit(1)
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        print_usage_exit(&args[0]);
    }

    let source_file = &args[1];
    let asm_file = "a.asm";
    let mut output_file = "a.out";

    let mut arg_i = 2;
    while arg_i < args.len() {
        match args[arg_i].as_str() {
            "-h" | "--help" => {
                print_usage_exit(&args[0]);
            }
            "-o" | "--output" => {
                assert!(arg_i + 1 < args.len(), "missing file for output parameter");
                output_file = &args[arg_i + 1];
                arg_i += 1;
            }
            _ => {
                eprintln!("ERROR: unknown argument: {}", args[arg_i]);
                std::process::exit(1);
            }
        }
        arg_i += 1;
    }

    let tokens = match lexer::lex(source_file) {
        Ok(tokens) => tokens,
        Err(_) => {
            println!("compilation failed (lexer error)");
            std::process::exit(1)
        }
    };
    dbg!(&tokens);

    //let ast = ASTNode::parse_root(&tokens);
    let ast = ASTProgram::parse(&tokens);
    dbg!(&ast);

    let func_map = match validate(&ast) {
        Ok(func_map) => func_map,
        Err(_) => {
            println!("compilation failed (validation error)");
            std::process::exit(1)
        }
    };

    let mut file = match File::create(asm_file) {
        Ok(f) => f,
        Err(e) => {
            println!("failed to open file: {} ({})", asm_file, e);
            std::process::exit(1)
        }
    };

    ASMGenerator::write_asm(&ast, func_map.clone(), &mut file).unwrap();
    ASMGenerator::write_asm(&ast, func_map, &mut io::stdout()).unwrap();

    file.flush().unwrap();

    let nasm_output = Command::new("nasm")
        .arg("-felf64")
        .arg(asm_file)
        .output().unwrap();

    if nasm_output.status.success() == false {
        let stderr = String::from_utf8(nasm_output.stderr).unwrap();
        println!("ERROR: nasm failed:\n{}", stderr);
        std::process::exit(1);
    }

    let gcc_output = Command::new("gcc")
        .arg("a.o")
        .arg(format!("-o{}", output_file))
        .output().unwrap();

    if gcc_output.status.success() == false {
        let stderr = String::from_utf8(gcc_output.stderr).unwrap();
        println!("ERROR: gcc failed:\n{}", stderr);
        std::process::exit(1);
    }
}
