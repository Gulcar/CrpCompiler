use crate::lexer::Tok;

#[derive(Debug)]
pub enum ASTNode {
    // program je ena funkcija
    Program(Box<ASTNode>),
    // funkcija je ime in en statement
    Function(String, Box<ASTNode>),
    // statement je expression
    Statement(Box<ASTNode>),
    // expression je int literal
    Expression(i32),
}

impl ASTNode {
    pub fn parse_root(tokens: &[Tok]) -> ASTNode {
        // TODO: for each function
        let func = Box::new(ASTNode::parse(tokens));
        return ASTNode::Program(func);
    }

    fn parse(tokens: &[Tok]) -> ASTNode {
        match tokens[0] {
            Tok::Func => {
                let ime = match tokens[1].clone() {
                    Tok::Identifier(id) => id,
                    _ => panic!("not an identifier")
                };
                assert_eq!(tokens[2], Tok::OpenParens);
                assert_eq!(tokens[3], Tok::CloseParens);
                assert_eq!(tokens[4], Tok::Int);
                assert_eq!(tokens[5], Tok::OpenBrace);

                let body = ASTNode::parse(&tokens[6..(tokens.len() - 1)]);

                assert_eq!(*tokens.last().unwrap(), Tok::CloseBrace);

                ASTNode::Function(ime, Box::new(body))
            },
            
            Tok::Return => {
                let expr = ASTNode::parse(&tokens[1..(tokens.len() - 1)]);
                assert_eq!(*tokens.last().unwrap(), Tok::Semicolon);

                ASTNode::Statement(Box::new(expr))
            },

            Tok::IntLiteral(val) => {
                assert_eq!(tokens.len(), 1);
                ASTNode::Expression(val)
            },

            _ => {
                ASTNode::Expression(-1)
            }
        }
    }
}
