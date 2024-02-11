use crate::lexer::Tok;

#[derive(Debug)]
pub struct ASTProgram {
    pub func: ASTFunction,
}

#[derive(Debug)]
pub struct ASTFunction {
    pub ime: String,
    pub statement: ASTStatement,
}

#[derive(Debug)]
pub struct ASTStatement {
    pub expr: ASTExpression,
}

#[derive(Debug)]
pub enum ASTExpression {
    Const(i32),
    UnaryOp(UnOp, Box<ASTExpression>),
    BinaryOp(BinOp, Box<ASTExpression>, Box<ASTExpression>),
}

#[derive(Debug)]
pub enum UnOp {
    Negation,
    BitwiseComplement,
    LogicalNegation,
}

#[derive(Debug)]
pub enum BinOp {
    Addition,
    Subtraction,
    Multiplication,
    Division,
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

        let body = ASTStatement::parse(&tokens[6..(tokens.len() - 1)]);

        assert_eq!(*tokens.last().unwrap(), Tok::CloseBrace);

        Self { ime, statement: body }
    }
}

impl ASTStatement {
    fn parse(tokens: &[Tok]) -> Self {
        assert_eq!(tokens[0], Tok::Return);
        let expr = ASTExpression::parse(&tokens[1..(tokens.len() - 1)]);
        assert_eq!(*tokens.last().unwrap(), Tok::Semicolon);
        Self { expr }
    }
}

impl ASTExpression {
    fn parse(tokens: &[Tok]) -> Self {

        if tokens.len() == 1 {
            match tokens[0] {
                Tok::IntLiteral(val) => { return ASTExpression::Const(val); }
                _ => panic!("should be an int literal {:?}", tokens[0]),
            }
        }

        let mut depth = 0;
        let mut ok_remove_parens = true;

        for (i, tok) in tokens.iter().enumerate().rev() {
            match tok {
                Tok::OpenParens => {
                    depth += 1;
                    if depth == 0 && i != 0 {
                        ok_remove_parens = false;
                    }
                }
                Tok::CloseParens => depth -= 1,
                Tok::Addition => if depth == 0 {
                    return ASTExpression::BinaryOp(
                        BinOp::Addition,
                        Box::new(ASTExpression::parse(&tokens[0..i])),
                        Box::new(ASTExpression::parse(&tokens[(i+1)..]))
                    );
                },
                Tok::Negation => if depth == 0 && i != 0 {
                    return ASTExpression::BinaryOp(
                        BinOp::Subtraction,
                        Box::new(ASTExpression::parse(&tokens[0..i])),
                        Box::new(ASTExpression::parse(&tokens[(i+1)..]))
                    );
                },
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

        for (i, tok) in tokens.iter().enumerate().rev() {
            match tok {
                Tok::OpenParens => depth += 1,
                Tok::CloseParens => depth -= 1,
                Tok::Multiplication => if depth == 0 {
                    return ASTExpression::BinaryOp(
                        BinOp::Multiplication,
                        Box::new(ASTExpression::parse(&tokens[0..i])),
                        Box::new(ASTExpression::parse(&tokens[(i+1)..]))
                    );
                },
                Tok::Division => if depth == 0 {
                    return ASTExpression::BinaryOp(
                        BinOp::Division,
                        Box::new(ASTExpression::parse(&tokens[0..i])),
                        Box::new(ASTExpression::parse(&tokens[(i+1)..]))
                    );
                },
                _ => {}
            }
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
