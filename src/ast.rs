use crate::lexer::Tok;

#[derive(Debug, PartialEq)]
pub struct ASTProgram {
    pub func: ASTFunction,
}

#[derive(Debug, PartialEq)]
pub struct ASTFunction {
    pub ime: String,
    pub statements: Vec<ASTStatement>,
}

#[derive(Debug, PartialEq)]
pub enum ASTStatement {
    // return <expr>;
    Return(ASTExpression),
    // ime spremenljivke in opcijski initializer vrednost
    Declaration(String, Option<ASTExpression>),
    // ime spremenljivke in nova vrednost
    Assignment(String, ASTExpression),
}

#[derive(Debug, PartialEq)]
pub enum ASTExpression {
    // samo stevilka
    Const(i32),
    // operator in desna stran
    UnaryOp(UnOp, Box<ASTExpression>),
    // operator, leva in desna stran
    BinaryOp(BinOp, Box<ASTExpression>, Box<ASTExpression>),
    // ime spremenljivke
    VarRef(String),
}

#[derive(Debug, PartialEq)]
pub enum UnOp {
    Negation,
    BitwiseComplement,
    LogicalNegation,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    LogicalAnd,
    LogicalOr,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Modulo,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,
}

impl ASTProgram {
    pub fn parse(tokens: &[Tok]) -> Self {
        let func = ASTFunction::parse(tokens);
        Self { func }
    }
}

impl ASTFunction {
    fn parse(tokens: &[Tok]) -> Self {
        assert_eq!(tokens[0], Tok::Func);
        let ime = match tokens[1].clone() {
            Tok::Identifier(id) => id,
            _ => panic!("not an identifier"),
        };
        assert_eq!(tokens[2], Tok::OpenParens);
        assert_eq!(tokens[3], Tok::CloseParens);
        assert_eq!(tokens[4], Tok::Int);
        assert_eq!(tokens[5], Tok::OpenBrace);

        let mut statements = Vec::new();
        let body_tokens = &tokens[6..(tokens.len() - 1)];
        for statement_tokens in body_tokens.split_inclusive(|x| *x == Tok::Semicolon) {
            statements.push(ASTStatement::parse(statement_tokens));
        }

        assert_eq!(*tokens.last().unwrap(), Tok::CloseBrace);

        Self { ime, statements }
    }
}

impl ASTStatement {
    fn parse(tokens: &[Tok]) -> Self {
        assert_eq!(*tokens.last().unwrap(), Tok::Semicolon);

        match tokens[0] {
            Tok::Return => {
                let expr = ASTExpression::parse(&tokens[1..(tokens.len() - 1)]);
                ASTStatement::Return(expr)
            }
            Tok::Int => {
                let var_name = match tokens[1] {
                    Tok::Identifier(ref name) => name,
                    _ => panic!("not a variable name {:?}", tokens[1]),
                };
                let expr = if tokens[2] == Tok::Assignment {
                    Some(ASTExpression::parse(&tokens[3..(tokens.len() - 1)]))
                } else {
                    assert_eq!(tokens.len(), 3);
                    None
                };
                ASTStatement::Declaration(var_name.clone(), expr)
            }
            Tok::Identifier(ref id) => {
                assert_eq!(tokens[1], Tok::Assignment);
                let expr = ASTExpression::parse(&tokens[2..(tokens.len() - 1)]);
                ASTStatement::Assignment(id.clone(), expr)
            }
            _ => {
                panic!("not a valid statement {:?}", tokens);
                //let expr = ASTExpression::parse(&tokens[0..(tokens.len() - 1)]);
                //ASTStatement::Expr(expr)
            }
        }
    }
}

impl ASTExpression {
    fn parse(tokens: &[Tok]) -> Self {

        if tokens.len() == 1 {
            match tokens[0] {
                Tok::IntLiteral(val) => { return ASTExpression::Const(val); }
                Tok::Identifier(ref id) => { return ASTExpression::VarRef(id.clone()); }
                _ => panic!("should be an int literal or a variable reference {:?}", tokens[0]),
            }
        }

        let mut depth = 0;
        let mut ok_remove_parens = true;

        // kje bi lahko splital
        let mut bin_ops: Vec<(usize, BinOp)> = Vec::new();

        for (i, tok) in tokens.iter().enumerate() {
            match tok {
                Tok::OpenParens => {
                    depth += 1;
                }
                Tok::CloseParens => {
                    depth -= 1;
                    if depth == 0 && i != tokens.len() - 1 {
                        ok_remove_parens = false;
                    }
                }

                Tok::Addition => if depth == 0 && i != 0 { bin_ops.push((i, BinOp::Addition)); },
                Tok::Negation => if depth == 0 && i != 0 { bin_ops.push((i, BinOp::Subtraction)); },
                Tok::Multiplication => if depth == 0 && i != 0 { bin_ops.push((i, BinOp::Multiplication)); },
                Tok::Division => if depth == 0 && i != 0 { bin_ops.push((i, BinOp::Division)); },
                Tok::And => if depth == 0 && i != 0 { bin_ops.push((i, BinOp::LogicalAnd)); },
                Tok::Or => if depth == 0 && i != 0 { bin_ops.push((i, BinOp::LogicalOr)); },
                Tok::Equal => if depth == 0 && i != 0 { bin_ops.push((i, BinOp::Equal)); },
                Tok::NotEqual => if depth == 0 && i != 0 { bin_ops.push((i, BinOp::NotEqual)); },
                Tok::LessThan => if depth == 0 && i != 0 { bin_ops.push((i, BinOp::LessThan)); },
                Tok::LessThanOrEqual => if depth == 0 && i != 0 { bin_ops.push((i, BinOp::LessThanOrEqual)); },
                Tok::GreaterThan => if depth == 0 && i != 0 { bin_ops.push((i, BinOp::GreaterThan)); },
                Tok::GreaterThanOrEqual => if depth == 0 && i != 0 { bin_ops.push((i, BinOp::GreaterThanOrEqual)); },
                Tok::Modulo => if depth == 0 && i != 0 { bin_ops.push((i, BinOp::Modulo)); },
                Tok::BitwiseAnd => if depth == 0 && i != 0 { bin_ops.push((i, BinOp::BitwiseAnd)); },
                Tok::BitwiseOr => if depth == 0 && i != 0 { bin_ops.push((i, BinOp::BitwiseOr)); },
                Tok::BitwiseXor => if depth == 0 && i != 0 { bin_ops.push((i, BinOp::BitwiseXor)); },
                Tok::ShiftLeft => if depth == 0 && i != 0 { bin_ops.push((i, BinOp::ShiftLeft)); },
                Tok::ShiftRight => if depth == 0 && i != 0 { bin_ops.push((i, BinOp::ShiftRight)); },
                _ => {}
            }
        }

        assert_eq!(depth, 0, "mismatching parentheses");

        if ok_remove_parens &&
            *tokens.first().unwrap() == Tok::OpenParens &&
            *tokens.last().unwrap() == Tok::CloseParens
        {
            return ASTExpression::parse(&tokens[1..(tokens.len() - 1)]);
        }

        if let Some((i, op)) = bin_ops.iter().rev().min_by_key(|x| x.1.precedence()) {
            return ASTExpression::BinaryOp(
                *op,
                Box::new(ASTExpression::parse(&tokens[0..(*i)])),
                Box::new(ASTExpression::parse(&tokens[(*i + 1)..]))
            );
        }

        match tokens[0] {
            Tok::Negation => {
                let expr = ASTExpression::parse(&tokens[1..]);
                ASTExpression::UnaryOp(UnOp::Negation, Box::new(expr))
            }
            Tok::BitwiseComplement => {
                let expr = ASTExpression::parse(&tokens[1..]);
                ASTExpression::UnaryOp(UnOp::BitwiseComplement, Box::new(expr))
            }
            Tok::LogicalNegation => {
                let expr = ASTExpression::parse(&tokens[1..]);
                ASTExpression::UnaryOp(UnOp::LogicalNegation, Box::new(expr))
            }
            _ => panic!("parse error ni unary expression {:?}", tokens[0]),
        }
    }
}

impl BinOp {
    fn precedence(&self) -> i32 {
        match *self {
            BinOp::LogicalOr => 0,
            BinOp::LogicalAnd => 1,
            BinOp::BitwiseOr => 2,
            BinOp::BitwiseXor => 3,
            BinOp::BitwiseAnd => 4,
            BinOp::Equal => 5,
            BinOp::NotEqual => 5,
            BinOp::LessThan => 6,
            BinOp::LessThanOrEqual => 6,
            BinOp::GreaterThan => 6,
            BinOp::GreaterThanOrEqual => 6,
            BinOp::ShiftLeft => 7,
            BinOp::ShiftRight => 7,
            BinOp::Addition => 8,
            BinOp::Subtraction => 8,
            BinOp::Multiplication => 9,
            BinOp::Division => 9,
            BinOp::Modulo => 9,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_main() {
        let tokens = [
            Tok::Func,
            Tok::Identifier("main".to_string()),
            Tok::OpenParens,
            Tok::CloseParens,
            Tok::Int,
            Tok::OpenBrace,
            Tok::Return,
            Tok::IntLiteral(2),
            Tok::Semicolon,
            Tok::CloseBrace,
        ];
        assert_eq!(
            ASTProgram::parse(&tokens),
            ASTProgram {
                func: ASTFunction {
                    ime: "main".to_string(),
                    statement: ASTStatement {
                        expr: ASTExpression::Const(2),
                    }
                }
            }
        );
    }

    #[test]
    fn test_ops() {
        let tokens = [
            Tok::Func,
            Tok::Identifier("ops".to_string()),
            Tok::OpenParens,
            Tok::CloseParens,
            Tok::Int,
            Tok::OpenBrace,
            Tok::Return,

            Tok::BitwiseComplement,
            Tok::IntLiteral(1),
            Tok::Negation,
            Tok::IntLiteral(2),
            Tok::Addition,
            Tok::IntLiteral(3),
            Tok::Multiplication,
            Tok::IntLiteral(5),
            Tok::Division,
            Tok::OpenParens,
            Tok::IntLiteral(3),
            Tok::Negation,
            Tok::IntLiteral(1),
            Tok::CloseParens,

            Tok::Semicolon,
            Tok::CloseBrace,
        ];
        assert_eq!(
            ASTProgram::parse(&tokens),
            ASTProgram {
                func: ASTFunction {
                    ime: "ops".to_string(),
                    statement: ASTStatement {
                        expr: ASTExpression::BinaryOp(
                            BinOp::Addition,
                            Box::new(ASTExpression::BinaryOp(
                                BinOp::Subtraction,
                                Box::new(ASTExpression::UnaryOp(
                                    UnOp::BitwiseComplement,
                                    Box::new(ASTExpression::Const(1))
                                )),
                                Box::new(ASTExpression::Const(2))
                            )),
                            Box::new(ASTExpression::BinaryOp(
                                BinOp::Division,
                                Box::new(ASTExpression::BinaryOp(
                                    BinOp::Multiplication,
                                    Box::new(ASTExpression::Const(3)),
                                    Box::new(ASTExpression::Const(5)),
                                )),
                                Box::new(ASTExpression::BinaryOp(
                                    BinOp::Subtraction,
                                    Box::new(ASTExpression::Const(3)),
                                    Box::new(ASTExpression::Const(1))
                                ))
                            ))
                        )
                    }
                }
            }
        );
    }

    #[test]
    fn test_logical() {
        let tokens = [
            Tok::Func,
            Tok::Identifier("main".to_string()),
            Tok::OpenParens,
            Tok::CloseParens,
            Tok::Int,
            Tok::OpenBrace,
            Tok::Return,

            Tok::OpenParens,
            Tok::IntLiteral(1),
            Tok::Or,
            Tok::IntLiteral(0),
            Tok::GreaterThanOrEqual,
            Tok::IntLiteral(2),
            Tok::CloseParens,
            Tok::And,
            Tok::IntLiteral(5),

            Tok::Semicolon,
            Tok::CloseBrace,
        ];
        assert_eq!(
            ASTProgram::parse(&tokens),
            ASTProgram {
                func: ASTFunction {
                    ime: "main".to_string(),
                    statement: ASTStatement {
                        expr: ASTExpression::BinaryOp(
                            BinOp::LogicalAnd,
                            Box::new(ASTExpression::BinaryOp(
                                BinOp::LogicalOr,
                                Box::new(ASTExpression::Const(1)),
                                Box::new(ASTExpression::BinaryOp(
                                    BinOp::GreaterThanOrEqual,
                                    Box::new(ASTExpression::Const(0)),
                                    Box::new(ASTExpression::Const(2))
                                ))
                            )),
                            Box::new(ASTExpression::Const(5))
                        )
                    }
                }
            }
        );
    }
}
