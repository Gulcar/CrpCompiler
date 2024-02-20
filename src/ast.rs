use crate::lexer::Tok;

#[derive(Debug, PartialEq)]
pub struct ASTProgram {
    pub functions: Vec<ASTFunction>,
}

#[derive(Debug, PartialEq)]
pub struct ASTFunction {
    pub ime: String,
    pub params: Vec<String>,
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
    // pogoj, if body, else body (mogoce prazen)
    IfElse(ASTExpression, Vec<ASTStatement>, Vec<ASTStatement>),
    // zacetek, pogoj, korak in body
    ForLoop(Box<ASTStatement>, ASTExpression, Box<ASTStatement>, Vec<ASTStatement>),
    // pogoj in body
    WhileLoop(ASTExpression, Vec<ASTStatement>),
    // body in pogoj
    DoWhileLoop(Vec<ASTStatement>, ASTExpression),
    Continue,
    Break,
    // klic funkcije ASTExpression::FunctionCall
    FunctionCall(ASTExpression),
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
    // ime funkcije in seznam argumentov
    FunctionCall(String, Vec<ASTExpression>),
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
        let mut start_token = 0;
        let mut functions = Vec::new();

        for (i, tok) in tokens.iter().enumerate().skip(1) {
            if *tok == Tok::Func {
                functions.push(ASTFunction::parse(&tokens[start_token..i]));
                start_token = i;
            }
            if i == tokens.len() - 1 {
                functions.push(ASTFunction::parse(&tokens[start_token..]));
            }
        }

        Self { functions }
    }
}

impl ASTFunction {
    fn parse(tokens: &[Tok]) -> Self {
        assert_eq!(tokens[0], Tok::Func);
        let ime = match tokens[1] {
            Tok::Identifier(ref id) => id.clone(),
            _ => panic!("not an identifier function name {:?}", tokens),
        };
        assert_eq!(tokens[2], Tok::OpenParens);

        let mut params = Vec::new();

        let mut i = 3;
        while tokens[i] == Tok::Int {
            let param = match tokens[i + 1] {
                Tok::Identifier(ref param) => param.clone(),
                _ => panic!("not an indentifier param {:?}", tokens),
            };
            params.push(param);
            if tokens[i + 2] == Tok::Comma {
                i += 3;
            } else {
                i += 2;
                break;
            }
        }

        assert_eq!(tokens[i], Tok::CloseParens);
        assert_eq!(tokens[i + 1], Tok::Int);
        assert_eq!(tokens[i + 2], Tok::OpenBrace);

        let body_tokens = &tokens[(i+3)..(tokens.len() - 1)];
        let statements = ASTStatement::parse_multiple(body_tokens);

        assert_eq!(*tokens.last().unwrap(), Tok::CloseBrace);

        Self { ime, statements, params }
    }
}

impl ASTStatement {
    fn parse_multiple(tokens: &[Tok]) -> Vec<ASTStatement> {
        let mut statements = Vec::new();

        let mut start_tok = 0;
        let mut depth = 0;

        let mut iter = tokens.iter().enumerate().peekable();
        while let Some((i, tok)) = iter.next() {
            if (*tok == Tok::Semicolon && depth == 0)
                || (*tok == Tok::CloseBrace && depth == 1 && iter.peek().map(|x| x.1) != Some(&Tok::Else) && tokens[start_tok] == Tok::If)
                || (*tok == Tok::CloseBrace && depth == 1 && (tokens[start_tok] == Tok::While || tokens[start_tok] == Tok::For))
                || (i == tokens.len() - 1)
            {
                statements.push(ASTStatement::parse(&tokens[start_tok..=i]));
                start_tok = i + 1;
            }
            if *tok == Tok::OpenBrace { depth += 1; }
            else if *tok == Tok::CloseBrace { depth -= 1; }
        }

        statements
    }

    fn parse(tokens: &[Tok]) -> Self {
        match tokens[0] {
            Tok::Return => {
                assert_eq!(*tokens.last().unwrap(), Tok::Semicolon);
                let expr = ASTExpression::parse(&tokens[1..(tokens.len() - 1)]);
                ASTStatement::Return(expr)
            }
            Tok::Int => {
                assert_eq!(*tokens.last().unwrap(), Tok::Semicolon);
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
                assert_eq!(*tokens.last().unwrap(), Tok::Semicolon);
                match tokens[1] {
                    Tok::Assignment => {
                        let expr = ASTExpression::parse(&tokens[2..(tokens.len() - 1)]);
                        ASTStatement::Assignment(id.clone(), expr)
                    }
                    Tok::AssignAdd => {
                        let expr = ASTExpression::parse(&tokens[2..(tokens.len() - 1)]);
                        let add_expr = ASTExpression::BinaryOp(
                            BinOp::Addition,
                            Box::new(ASTExpression::VarRef(id.clone())),
                            Box::new(expr)
                        );
                        ASTStatement::Assignment(id.clone(), add_expr)
                    }
                    Tok::AssignSub => {
                        let expr = ASTExpression::parse(&tokens[2..(tokens.len() - 1)]);
                        let sub_expr = ASTExpression::BinaryOp(
                            BinOp::Subtraction,
                            Box::new(ASTExpression::VarRef(id.clone())),
                            Box::new(expr)
                        );
                        ASTStatement::Assignment(id.clone(), sub_expr)
                    }
                    Tok::AssignMul => {
                        let expr = ASTExpression::parse(&tokens[2..(tokens.len() - 1)]);
                        let mul_expr = ASTExpression::BinaryOp(
                            BinOp::Multiplication,
                            Box::new(ASTExpression::VarRef(id.clone())),
                            Box::new(expr)
                        );
                        ASTStatement::Assignment(id.clone(), mul_expr)
                    }
                    Tok::AssignDiv => {
                        let expr = ASTExpression::parse(&tokens[2..(tokens.len() - 1)]);
                        let div_expr = ASTExpression::BinaryOp(
                            BinOp::Division,
                            Box::new(ASTExpression::VarRef(id.clone())),
                            Box::new(expr)
                        );
                        ASTStatement::Assignment(id.clone(), div_expr)
                    }
                    Tok::OpenParens => {
                        let call_expr = ASTExpression::parse(&tokens[0..(tokens.len() - 1)]);
                        match call_expr {
                            ASTExpression::FunctionCall(_, _) => {},
                            _ => panic!("not a function call {:?}", tokens),
                        }
                        ASTStatement::FunctionCall(call_expr)
                    }
                    _ => panic!("invalid assignment or function call {:?}", tokens[1])
                }
            }
            Tok::If => {
                assert_eq!(*tokens.last().unwrap(), Tok::CloseBrace);

                let mut body_start = None;
                let mut body_end = None;
                let mut depth = 0;

                for (i, tok) in tokens.iter().enumerate() {
                    match *tok {
                        Tok::OpenBrace => {
                            depth += 1;
                            if body_start == None && depth == 1 {
                                body_start = Some(i);
                            }
                        }
                        Tok::CloseBrace => {
                            depth -= 1;
                            if body_end == None && depth == 0 {
                                body_end = Some(i);
                            }
                        }
                        _ => {}
                    }
                }
                let body_start = body_start.unwrap();
                let body_end = body_end.unwrap();

                let cond = ASTExpression::parse(&tokens[1..body_start]);
                let body = ASTStatement::parse_multiple(&tokens[(body_start+1)..body_end]);
                let else_body = if tokens.get(body_end + 1) == Some(&Tok::Else) {
                    let (else_start, else_end) = if tokens[body_end + 2] == Tok::OpenBrace {
                        (body_end + 3, tokens.len() - 1)
                    } else {
                        (body_end + 2, tokens.len())
                    };
                    ASTStatement::parse_multiple(&tokens[else_start..else_end])
                } else {
                    Vec::new()
                };
                ASTStatement::IfElse(cond, body, else_body)
            }
            Tok::While => {
                assert_eq!(*tokens.last().unwrap(), Tok::CloseBrace);
                let body_start = tokens.iter().position(|t| *t == Tok::OpenBrace).unwrap();
                let cond = ASTExpression::parse(&tokens[1..body_start]);
                let statements = ASTStatement::parse_multiple(&tokens[(body_start+1)..(tokens.len()-1)]);
                ASTStatement::WhileLoop(cond, statements)
            }
            Tok::Do => {
                assert_eq!(tokens[1], Tok::OpenBrace);
                assert_eq!(*tokens.last().unwrap(), Tok::Semicolon);
                let body_end = tokens.iter().enumerate().rev().find(|(_, t)| **t == Tok::CloseBrace).map(|(p, _)| p).unwrap();
                assert_eq!(tokens[body_end + 1], Tok::While);
                let cond = ASTExpression::parse(&tokens[(body_end + 2)..(tokens.len() - 1)]);
                let statements = ASTStatement::parse_multiple(&tokens[2..body_end]);
                ASTStatement::DoWhileLoop(statements, cond)
            }
            Tok::For => {
                let iter_var_name = match tokens[1] {
                    Tok::Identifier(ref name) => name.clone(),
                    _ => panic!("should be a variable name in for loop {:?}", tokens),
                };
                assert_eq!(tokens[2], Tok::In);
                let start = ASTExpression::parse(&tokens[3..=3]);
                assert_eq!(tokens[4], Tok::DotDot);
                let end = ASTExpression::parse(&tokens[5..=5]);
                assert_eq!(tokens[6], Tok::OpenBrace);
                assert_eq!(*tokens.last().unwrap(), Tok::CloseBrace);

                let init = ASTStatement::Declaration(iter_var_name.clone(), Some(start));
                let cond = ASTExpression::BinaryOp(
                    BinOp::LessThan,
                    Box::new(ASTExpression::VarRef(iter_var_name.clone())),
                    Box::new(end)
                );
                let step = ASTStatement::Assignment(
                    iter_var_name.clone(),
                    ASTExpression::BinaryOp(
                        BinOp::Addition,
                        Box::new(ASTExpression::VarRef(iter_var_name)),
                        Box::new(ASTExpression::Const(1))
                    )
                );
                let body = ASTStatement::parse_multiple(&tokens[7..(tokens.len()-1)]);
                ASTStatement::ForLoop(Box::new(init), cond, Box::new(step), body)
            }
            Tok::Break => {
                assert_eq!(tokens.len(), 2);
                assert_eq!(tokens[1], Tok::Semicolon);
                ASTStatement::Break
            }
            Tok::Continue => {
                assert_eq!(tokens.len(), 2);
                assert_eq!(tokens[1], Tok::Semicolon);
                ASTStatement::Continue
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
            Tok::Identifier(ref func) => {
                assert_eq!(tokens[1], Tok::OpenParens);
                assert_eq!(*tokens.last().unwrap(), Tok::CloseParens);
                let mut args = Vec::new();
                let arg_tokens = &tokens[2..(tokens.len() - 1)];
                for a in arg_tokens.split(|t| *t == Tok::Comma) {
                    args.push(ASTExpression::parse(a));
                }
                ASTExpression::FunctionCall(func.clone(), args)
            }
            _ => panic!("parse error ni unary expression ali function call {:?}", tokens[0]),
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
            Tok::Int,
            Tok::Identifier("var".to_string()),
            Tok::Semicolon,
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
                    statements: vec![
                        ASTStatement::Declaration("var".to_string(), None),
                        ASTStatement::Return(ASTExpression::Const(2)),
                    ]
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
                    statements: vec![ASTStatement::Return(
                        ASTExpression::BinaryOp(
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
                    )]
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
                    statements: vec![ASTStatement::Return(
                        ASTExpression::BinaryOp(
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
                    )]
                }
            }
        );
    }
}
