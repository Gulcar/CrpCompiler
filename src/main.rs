use std::env;

mod lexer;
mod ast;

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

    for arg in &args[2..] {
        match arg.as_str() {
            "-v" | "--verbose" => {
                println!("pozdralvjen svet!");
            }
            _ => {
                eprintln!("ERROR: unknown argument: {}", arg);
                std::process::exit(1);
            }
        }
    }

    let tokens = match lexer::lex(&args[1]) {
        Ok(tokens) => tokens,
        Err(_) => {
            println!("compilation failed (lexer error)");
            std::process::exit(1)
        }
    };

    dbg!(&tokens);

    let ast = ASTNode::parse_root(&tokens);

    dbg!(&ast);
}
