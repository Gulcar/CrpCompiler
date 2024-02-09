use std::io::{self, Write};
use crate::ast::*;

pub fn write_asm<W: Write>(ast_node: &ASTProgram, f: &mut W) -> io::Result<()> {
    writeln!(f, "\t\tglobal main")?;
    writeln!(f, "\t\tsection .text")?;
    write_asm_function(&ast_node.func, f)?;
    Ok(())
}

fn write_asm_function<W: Write>(ast_node: &ASTFunction, f: &mut W) -> io::Result<()> {
    writeln!(f, "{}:", ast_node.ime)?;
    write_asm_statement(&ast_node.statement, f)?;
    Ok(())
}

fn write_asm_statement<W: Write>(ast_node: &ASTStatement, f: &mut W) -> io::Result<()> {
    write_asm_expression(&ast_node.expr, f)?;
    writeln!(f, "\t\tret")?;
    Ok(())
}

fn write_asm_expression<W: Write>(ast_node: &ASTExpression, f: &mut W) -> io::Result<()> {
    match ast_node {
        ASTExpression::Const(val) => {
            writeln!(f, "\t\tmov rax, {}", val)?;
        },
        ASTExpression::UnaryOp(op, expr) => {
            write_asm_expression(expr, f)?;
        }
    }
    Ok(())
}

/*
pub fn write_asm<W: Write>(ast_node: &ASTProgram, f: &mut W) -> io::Result<()> {
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
        ASTNode::ExpressionConst(val) => {
            writeln!(f, "\t\tmov rax, {}", val)?;
        }
        _ => {
            panic!("write_asm napaka");
        }
    }

    Ok(())
}
*/
