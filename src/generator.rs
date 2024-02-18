use crate::ast::*;
use std::{collections::HashMap, io::{self, Write}};

pub struct ASMGenerator {
    stack_index: i32,
    next_label_id: u32,
}

impl ASMGenerator {
    fn new() -> Self {
        Self {
            stack_index: 0,
            next_label_id: 0,
        }
    }

    pub fn write_asm<W: Write>(ast_node: &ASTProgram, f: &mut W) -> io::Result<()> {
        let mut generator = ASMGenerator::new();
        writeln!(f, "\t\tglobal main")?;
        writeln!(f, "\t\tsection .text")?;
        generator.write_asm_function(&ast_node.func, f)?;
        Ok(())
    }

    fn write_asm_function<W: Write>(&mut self, ast_node: &ASTFunction, f: &mut W) -> io::Result<()> {
        writeln!(f, "{}:", ast_node.ime)?;

        // function prologue
        writeln!(f, "\t\tpush rbp")?;
        writeln!(f, "\t\tmov rbp, rsp")?;
        self.stack_index = 0;

        let mut var_map = HashMap::new();

        for statement in &ast_node.statements {
            self.write_asm_statement(statement, &mut var_map, f)?;
        }

        Ok(())
    }

    fn write_asm_statement<W: Write>(
        &mut self,
        ast_node: &ASTStatement,
        var_map: &mut HashMap<String, i32>,
        f: &mut W
    ) -> io::Result<()>
    {
        match ast_node {
            ASTStatement::Return(expr) => {
                self.write_asm_expression(expr, var_map, f)?;
                // function epilogue
                writeln!(f, "\t\tmov rsp, rbp")?;
                writeln!(f, "\t\tpop rbp")?;
                writeln!(f, "\t\tret")?;
            }
            ASTStatement::Declaration(ime, opt_expr) => {
                if var_map.contains_key(ime) {
                    panic!("variable '{}' declared twice", ime);
                }
                if let Some(expr) = opt_expr {
                    self.write_asm_expression(expr, var_map, f)?;
                }
                writeln!(f, "\t\tpush rax")?;
                self.stack_index -= 8;
                var_map.insert(ime.clone(), self.stack_index);
            }
            ASTStatement::Assignment(ime, expr) => {
                self.write_asm_expression(expr, var_map, f)?;
                let offset = var_map.get(ime).expect("use of undeclared variable");
                writeln!(f, "\t\tmov qword [rbp - {}], rax", -offset)?;
            }
            ASTStatement::IfElse(cond, if_body, else_body) => {
                let label_id = self.create_label_id();

                self.write_asm_expression(cond, var_map, f)?;
                writeln!(f, "\t\ttest rax, rax")?;
                writeln!(f, "\t\tje crp_else_{}", label_id)?;

                let mut if_var_scope = var_map.clone();
                for statement in if_body {
                    self.write_asm_statement(statement, &mut if_var_scope, f)?;
                }
                if else_body.len() > 0 {
                    writeln!(f, "\t\tjmp crp_end_else_{}", label_id)?;
                }

                writeln!(f, "crp_else_{}:", label_id)?;

                if else_body.len() > 0 {
                    let mut else_var_scope = var_map.clone();
                    for statement in else_body {
                        self.write_asm_statement(statement, &mut else_var_scope, f)?;
                    }
                    writeln!(f, "crp_end_else_{}:", label_id)?;
                }
            }
        }
        Ok(())
    }

    fn write_asm_expression<W: Write>(
        &mut self,
        ast_node: &ASTExpression,
        var_map: &mut HashMap<String, i32>,
        f: &mut W
    ) -> io::Result<()>
    {
        match ast_node {
            ASTExpression::Const(val) => {
                writeln!(f, "\t\tmov rax, {}", val)?;
            }
            ASTExpression::UnaryOp(op, expr) => match op {
                UnOp::Negation => {
                    self.write_asm_expression(expr, var_map, f)?;
                    writeln!(f, "\t\tneg rax")?;
                }
                UnOp::BitwiseComplement => {
                    self.write_asm_expression(expr, var_map, f)?;
                    writeln!(f, "\t\tnot rax")?;
                }
                UnOp::LogicalNegation => {
                    self.write_asm_expression(expr, var_map, f)?;
                    writeln!(f, "\t\ttest rax, rax")?;
                    writeln!(f, "\t\tmov rax, 0")?; // mov da ne spremenim test flagov
                    writeln!(f, "\t\tsete al")?; // set na 1 ce je prejsni test equal
                }
            }
            ASTExpression::BinaryOp(op, left, right) => match op {
                BinOp::Addition => {
                    self.write_asm_expression(left, var_map, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    self.write_asm_expression(right, var_map, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\tadd rax, rcx")?;
                }
                BinOp::Subtraction => {
                    self.write_asm_expression(right, var_map, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    self.write_asm_expression(left, var_map, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\tsub rax, rcx")?;
                }
                BinOp::Multiplication => {
                    self.write_asm_expression(left, var_map, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    self.write_asm_expression(right, var_map, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\timul rax, rcx")?;
                }
                BinOp::Division => {
                    self.write_asm_expression(right, var_map, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    self.write_asm_expression(left, var_map, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\tcqo")?;
                    writeln!(f, "\t\tidiv rcx")?;
                }
                BinOp::Equal => {
                    self.write_asm_expression(left, var_map, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    self.write_asm_expression(right, var_map, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\tcmp rax, rcx")?;
                    writeln!(f, "\t\tmov rax, 0")?;
                    writeln!(f, "\t\tsete al")?;
                }
                BinOp::NotEqual => {
                    self.write_asm_expression(left, var_map, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    self.write_asm_expression(right, var_map, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\tcmp rax, rcx")?;
                    writeln!(f, "\t\tmov rax, 0")?;
                    writeln!(f, "\t\tsetne al")?;
                }
                BinOp::LessThan => {
                    self.write_asm_expression(left, var_map, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    self.write_asm_expression(right, var_map, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\tcmp rax, rcx")?;
                    writeln!(f, "\t\tmov rax, 0")?;
                    writeln!(f, "\t\tsetg al")?;
                }
                BinOp::LessThanOrEqual => {
                    self.write_asm_expression(left, var_map, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    self.write_asm_expression(right, var_map, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\tcmp rax, rcx")?;
                    writeln!(f, "\t\tmov rax, 0")?;
                    writeln!(f, "\t\tsetge al")?;
                }
                BinOp::GreaterThan => {
                    self.write_asm_expression(left, var_map, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    self.write_asm_expression(right, var_map, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\tcmp rax, rcx")?;
                    writeln!(f, "\t\tmov rax, 0")?;
                    writeln!(f, "\t\tsetl al")?;
                }
                BinOp::GreaterThanOrEqual => {
                    self.write_asm_expression(left, var_map, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    self.write_asm_expression(right, var_map, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\tcmp rax, rcx")?;
                    writeln!(f, "\t\tmov rax, 0")?;
                    writeln!(f, "\t\tsetle al")?;
                }
                BinOp::LogicalOr => {
                    let label_id = self.create_label_id();
                    self.write_asm_expression(left, var_map, f)?;
                    writeln!(f, "\t\ttest rax, rax")?;
                    writeln!(f, "\t\tjne crp_or_end_{}", label_id)?;
                    self.write_asm_expression(right, var_map, f)?;
                    writeln!(f, "crp_or_end_{}:", label_id)?;
                    writeln!(f, "\t\ttest rax, rax")?;
                    writeln!(f, "\t\tmov rax, 0")?;
                    writeln!(f, "\t\tsetne al")?;
                }
                BinOp::LogicalAnd => {
                    let label_id = self.create_label_id();
                    self.write_asm_expression(left, var_map, f)?;
                    writeln!(f, "\t\ttest rax, rax")?;
                    writeln!(f, "\t\tje crp_and_end_{}", label_id)?;
                    self.write_asm_expression(right, var_map, f)?;
                    writeln!(f, "crp_and_end_{}:", label_id)?;
                    writeln!(f, "\t\ttest rax, rax")?;
                    writeln!(f, "\t\tmov rax, 0")?;
                    writeln!(f, "\t\tsetne al")?;
                }
                BinOp::Modulo => {
                    self.write_asm_expression(right, var_map, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    self.write_asm_expression(left, var_map, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\tcqo")?;
                    writeln!(f, "\t\tidiv rcx")?;
                    writeln!(f, "\t\tmov rax, rdx")?;
                }
                BinOp::BitwiseAnd => {
                    self.write_asm_expression(left, var_map, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    self.write_asm_expression(right, var_map, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\tand rax, rcx")?;
                }
                BinOp::BitwiseOr => {
                    self.write_asm_expression(left, var_map, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    self.write_asm_expression(right, var_map, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\tor rax, rcx")?;
                }
                BinOp::BitwiseXor => {
                    self.write_asm_expression(left, var_map, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    self.write_asm_expression(right, var_map, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\txor rax, rcx")?;
                }
                BinOp::ShiftLeft => {
                    self.write_asm_expression(right, var_map, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    self.write_asm_expression(left, var_map, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\tshl rax, cl")?;
                }
                BinOp::ShiftRight => {
                    self.write_asm_expression(right, var_map, f)?;
                    writeln!(f, "\t\tpush rax")?;
                    self.write_asm_expression(left, var_map, f)?;
                    writeln!(f, "\t\tpop rcx")?;
                    writeln!(f, "\t\tshr rax, cl")?;
                }
            }
            ASTExpression::VarRef(ime) => {
                let offset = var_map.get(ime).expect("use of undeclared variable");
                writeln!(f, "\t\tmov rax, qword [rbp - {}]", -offset)?;
            }
        }
        Ok(())
    }

    fn create_label_id(&mut self) -> u32 {
        self.next_label_id += 1;
        self.next_label_id
    }
}
