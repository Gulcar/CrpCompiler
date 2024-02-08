use std::io::{self, Write};

use crate::ASTNode;

pub fn write_asm<W: Write>(ast_node: &ASTNode, f: &mut W) -> io::Result<()> {
    match ast_node {
        ASTNode::Program(func) => {
            writeln!(f, "\t\tglobal main")?;
            writeln!(f, "\t\tsection .text")?;
            write_asm(func, f)?;
        },
        ASTNode::Function(ime, statement) => {
            writeln!(f, "{}:", ime)?;
            write_asm(statement, f)?;
        },
        ASTNode::Statement(expr) => {
            write_asm(expr, f)?;
            writeln!(f, "\t\tret")?;
        },
        ASTNode::Expression(val) => {
            writeln!(f, "\t\tmov rax, {}", val)?;
        }
    }

    Ok(())
}

