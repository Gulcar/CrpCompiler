use std::fs::File;
use std::io::{BufRead, BufReader};
use std::num::IntErrorKind;

#[derive(Debug, PartialEq)]
pub enum Tok {
    Identifier(String),
    IntLiteral(i32),
    Func,
    Int,
    Return,
    OpenParens,
    CloseParens,
    OpenBrace,
    CloseBrace,
    Semicolon,
}

#[derive(Debug, PartialEq)]
enum ParseErrorKind {
    InvalidDigit,
    InvalidCharacter,
}

#[derive(Debug, PartialEq)]
struct ParseError {
    kind: ParseErrorKind,
    word: String,
}

pub fn lex(file_name: &str) -> Result<Vec<Tok>, ()> {
    let mut vec = Vec::new();

    let file = File::open(file_name).unwrap();
    let reader = BufReader::new(file);

    let mut line_num = 1;
    let mut ok = true;

    for line in reader.lines() {
        let line = line.unwrap();

        match lex_line(&line, &mut vec) {
            Ok(_) => {},
            Err(e) => {
                println!("error parsing line {} ({:?}):\n{}", line_num, e, line);
                ok = false;
            }
        }

        line_num += 1;
    }

    if ok {
        Ok(vec)
    } else {
        Err(())
    }
}

fn lex_line(line: &str, vec: &mut Vec<Tok>) -> Result<(), ParseError> {
    let mut trenutna_beseda = String::new();

    for c in line.chars() {
        if c == '#' {
            if trenutna_beseda.len() > 0 {
                vec.push(word_to_tok(trenutna_beseda)?);
            }
            return Ok(());
        }
        if c.is_whitespace() {
            if trenutna_beseda.len() > 0 {
                vec.push(word_to_tok(trenutna_beseda)?);
                trenutna_beseda = String::new();
            }
            continue;
        }

        let en_char = match c {
            '(' => Some(Tok::OpenParens),
            ')' => Some(Tok::CloseParens),
            '{' => Some(Tok::OpenBrace),
            '}' => Some(Tok::CloseBrace),
            ';' => Some(Tok::Semicolon),
            _ => None,
        };

        if let Some(tok) = en_char {
            if trenutna_beseda.len() > 0 {
                vec.push(word_to_tok(trenutna_beseda)?);
                trenutna_beseda = String::new();
            }
            vec.push(tok);
        }
        else {
            trenutna_beseda.push(c);
        }
    }

    if trenutna_beseda.len() > 0 {
        vec.push(word_to_tok(trenutna_beseda)?);
    }

    Ok(())
}

fn word_to_tok(word: String) -> Result<Tok, ParseError> {
    match word.as_str() {
        "func" => return Ok(Tok::Func),
        "int" => return Ok(Tok::Int),
        "return" => return Ok(Tok::Return),
        _ => {}
    }

    if word.chars().nth(0).unwrap().is_numeric() {
        match word.parse() {
            Ok(val) => return Ok(Tok::IntLiteral(val)),
            Err(e) => { 
                match e.kind() {
                    IntErrorKind::InvalidDigit => Err(ParseError {
                        kind: ParseErrorKind::InvalidDigit,
                        word
                    }),
                    _ => panic!("error: {:?}", e),
                }
            }
        }
    }
    else {
        if word.chars().all(|c| c.is_ascii_alphabetic() || c.is_ascii_digit()) {
            return Ok(Tok::Identifier(word));
        }
        else {
            return Err(ParseError {
                kind: ParseErrorKind::InvalidCharacter,
                word
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_main() {
        let line = "func main() int { return 2; }";
        let expect: Vec<Tok> = vec![
            Tok::Func,
            Tok::Identifier("main".to_string()),
            Tok::OpenParens,
            Tok::CloseParens,
            Tok::Int,
            Tok::OpenBrace,
            Tok::Return,
            Tok::IntLiteral(2),
            Tok::Semicolon,
            Tok::CloseBrace
        ];
        let mut lex = Vec::new();
        let result = lex_line(line, &mut lex);
        assert_eq!(result, Ok(()));
        assert_eq!(lex, expect);
    }

    #[test]
    fn test_skupaj() {
        let line = "func moj(int a)int{return 2;}";
        let expect: Vec<Tok> = vec![
            Tok::Func,
            Tok::Identifier("moj".to_string()),
            Tok::OpenParens,
            Tok::Int,
            Tok::Identifier("a".to_string()),
            Tok::CloseParens,
            Tok::Int,
            Tok::OpenBrace,
            Tok::Return,
            Tok::IntLiteral(2),
            Tok::Semicolon,
            Tok::CloseBrace
        ];
        let mut lex = Vec::new();
        let result = lex_line(line, &mut lex);
        assert_eq!(result, Ok(()));
        assert_eq!(lex, expect);
    }

    #[test]
    fn test_error() {
        let line = "func main() int { return 2a; }";
        let mut lex = Vec::new();
        let result = lex_line(line, &mut lex);
        assert_eq!(result, Err(ParseError {
            kind: ParseErrorKind::InvalidDigit,
            word: "2a".to_string()
        }));
    }
}

