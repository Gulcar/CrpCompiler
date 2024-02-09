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
}

#[derive(Debug)]
pub enum UnOp {
    Negation,
    BitwiseComplement,
    LogicalNegation,
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
            _ => panic!("not an identifier")
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
        match tokens[0] {
            Tok::IntLiteral(val) => {
                assert_eq!(tokens.len(), 1);
                ASTExpression::Const(val)
            },
            Tok::Negation => {
                let expr = ASTExpression::parse(&tokens[1..]);
                ASTExpression::UnaryOp(UnOp::Negation, Box::new(expr))
            },
            Tok::BitwiseComplement => {
                let expr = ASTExpression::parse(&tokens[1..]);
                ASTExpression::UnaryOp(UnOp::BitwiseComplement, Box::new(expr))
            },
            Tok::LogicalNegation => {
                let expr = ASTExpression::parse(&tokens[1..]);
                ASTExpression::UnaryOp(UnOp::LogicalNegation, Box::new(expr))
            },
            _ => panic!("parse error ni expression")
        }
    }
}
