use crate::ast::*;
use std::io::{self, Write};

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
        }
        ASTExpression::UnaryOp(op, expr) => {
            match op {
                UnOp::Negation => {
                    write_asm_expression(expr, f)?;
                    writeln!(f, "\t\tneg rax")?;
                }
                UnOp::BitwiseComplement => {
                    write_asm_expression(expr, f)?;
                    writeln!(f, "\t\tnot rax")?;
                }
                UnOp::LogicalNegation => {
                    write_asm_expression(expr, f)?;
                    writeln!(f, "\t\ttest rax, rax")?;
                    writeln!(f, "\t\tmov rax, 0")?; // mov da ne spremenim test flagov
                    writeln!(f, "\t\tsete al")?; // set na 1 ce je prejsni test equal
                }
            }
        }
        ASTExpression::BinaryOp(op, left, right) => {
            match op {
                BinOp::Addition => {
                    write_asm_expression(left, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    write_asm_expression(right, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\tadd rax, rcx")?;
                }
                BinOp::Subtraction => {
                    write_asm_expression(right, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    write_asm_expression(left, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\tsub rax, rcx")?;
                }
                BinOp::Multiplication => {
                    write_asm_expression(left, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    write_asm_expression(right, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\timul rax, rcx")?;
                }
                BinOp::Division => {
                    write_asm_expression(right, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    write_asm_expression(left, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\tcqo")?;
                    writeln!(f, "\t\tidiv rcx")?;
                }
            }
        }
    }
    Ok(())
}

