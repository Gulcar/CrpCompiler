use crate::ast::*;
use std::{io::{self, Write}, sync::atomic::{AtomicU32, Ordering}};

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
                BinOp::Equal => {
                    write_asm_expression(left, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    write_asm_expression(right, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\tcmp rax, rcx")?;
                    writeln!(f, "\t\tmov rax, 0")?;
                    writeln!(f, "\t\tsete al")?;
                }
                BinOp::NotEqual => {
                    write_asm_expression(left, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    write_asm_expression(right, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\tcmp rax, rcx")?;
                    writeln!(f, "\t\tmov rax, 0")?;
                    writeln!(f, "\t\tsetne al")?;
                }
                BinOp::LessThan => {
                    write_asm_expression(left, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    write_asm_expression(right, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\tcmp rax, rcx")?;
                    writeln!(f, "\t\tmov rax, 0")?;
                    writeln!(f, "\t\tsetg al")?;
                }
                BinOp::LessThanOrEqual => {
                    write_asm_expression(left, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    write_asm_expression(right, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\tcmp rax, rcx")?;
                    writeln!(f, "\t\tmov rax, 0")?;
                    writeln!(f, "\t\tsetge al")?;
                }
                BinOp::GreaterThan => {
                    write_asm_expression(left, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    write_asm_expression(right, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\tcmp rax, rcx")?;
                    writeln!(f, "\t\tmov rax, 0")?;
                    writeln!(f, "\t\tsetl al")?;
                }
                BinOp::GreaterThanOrEqual => {
                    write_asm_expression(left, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    write_asm_expression(right, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\tcmp rax, rcx")?;
                    writeln!(f, "\t\tmov rax, 0")?;
                    writeln!(f, "\t\tsetle al")?;
                }
                BinOp::LogicalOr => {
                    let label_id = create_label_id();
                    write_asm_expression(left, f)?;
                    writeln!(f, "\t\ttest rax, rax")?;
                    writeln!(f, "\t\tjne crp_or_end_{}", label_id)?;
                    write_asm_expression(right, f)?;
                    writeln!(f, "crp_or_end_{}:", label_id)?;
                    writeln!(f, "\t\ttest rax, rax")?;
                    writeln!(f, "\t\tmov rax, 0")?;
                    writeln!(f, "\t\tsetne al")?;
                }
                BinOp::LogicalAnd => {
                    let label_id = create_label_id();
                    write_asm_expression(left, f)?;
                    writeln!(f, "\t\ttest rax, rax")?;
                    writeln!(f, "\t\tje crp_and_end_{}", label_id)?;
                    write_asm_expression(right, f)?;
                    writeln!(f, "crp_and_end_{}:", label_id)?;
                    writeln!(f, "\t\ttest rax, rax")?;
                    writeln!(f, "\t\tmov rax, 0")?;
                    writeln!(f, "\t\tsetne al")?;
                }
                BinOp::Modulo => {
                    write_asm_expression(right, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    write_asm_expression(left, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\tcqo")?;
                    writeln!(f, "\t\tidiv rcx")?;
                    writeln!(f, "\t\tmov rax, rdx")?;
                }
                BinOp::BitwiseAnd => {
                    write_asm_expression(left, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    write_asm_expression(right, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\tand rax, rcx")?;
                }
                BinOp::BitwiseOr => {
                    write_asm_expression(left, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    write_asm_expression(right, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\tor rax, rcx")?;
                }
                BinOp::BitwiseXor => {
                    write_asm_expression(left, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    write_asm_expression(right, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\txor rax, rcx")?;
                }
                BinOp::ShiftLeft => {
                    write_asm_expression(right, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    write_asm_expression(left, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\tshl rax, cl")?;
                }
                BinOp::ShiftRight => {
                    write_asm_expression(right, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    write_asm_expression(left, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\tshr rax, cl")?;
                }
            }
        }
    }
    Ok(())
}

fn create_label_id() -> u32 {
    static COUNTER: AtomicU32 = AtomicU32::new(0);
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

