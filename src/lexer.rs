use std::fs::File;
use std::io::{BufRead, BufReader};
use std::num::IntErrorKind;
use std::str::Chars;

#[derive(Debug, PartialEq, Clone)]
pub enum Tok {
    Identifier(String),
    IntLiteral(i32),
    Func,
    Int,
    Return,
    OpenParens,        // (
    CloseParens,       // )
    OpenBrace,         // {
    CloseBrace,        // }
    Semicolon,         // ;
    Negation,          // -
    BitwiseComplement, // ~
    LogicalNegation,   // !
    Addition,          // +
    Multiplication,    // *
    Division,          // /
    And,               // &&
    Or,                // ||
    Equal,             // ==
    NotEqual,          // !=
    LessThan,          // <
    LessThanOrEqual,   // <=
    GreaterThan,       // >
    GreaterThanOrEqual,// >=
}

#[derive(Debug, PartialEq)]
enum LexErrorKind {
    InvalidDigit,
    InvalidCharacter,
}

#[derive(Debug, PartialEq)]
struct LexError {
    kind: LexErrorKind,
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
            Ok(_) => {}
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

fn check_two_chars(c1: char, c2: char, curr_char: char, chars: &mut Chars) -> bool {
    if curr_char == c1 {
        if let Some(next_char) = chars.clone().next() {
            if next_char == c2 {
                chars.next(); // consume 1 character
                return true;
            }
        }
    }
    false
}

fn lex_line(line: &str, vec: &mut Vec<Tok>) -> Result<(), LexError> {
    let mut trenutna_beseda = String::new();

    let mut chars = line.chars();
    while let Some(c) = chars.next() {

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

        let dva_char;
        if check_two_chars('&', '&', c, &mut chars) { dva_char = Some(Tok::And); }
        else if check_two_chars('|', '|', c, &mut chars) { dva_char = Some(Tok::Or); }
        else if check_two_chars('=', '=', c, &mut chars) { dva_char = Some(Tok::Equal); }
        else if check_two_chars('!', '=', c, &mut chars) { dva_char = Some(Tok::NotEqual); }
        else if check_two_chars('<', '=', c, &mut chars) { dva_char = Some(Tok::LessThanOrEqual); }
        else if check_two_chars('>', '=', c, &mut chars) { dva_char = Some(Tok::GreaterThanOrEqual); }
        else { dva_char = None; }

        if let Some(tok) = dva_char {
            if trenutna_beseda.len() > 0 {
                vec.push(word_to_tok(trenutna_beseda)?);
                trenutna_beseda = String::new();
            }
            vec.push(tok);
            continue;
        }

        let en_char = match c {
            '(' => Some(Tok::OpenParens),
            ')' => Some(Tok::CloseParens),
            '{' => Some(Tok::OpenBrace),
            '}' => Some(Tok::CloseBrace),
            ';' => Some(Tok::Semicolon),
            '-' => Some(Tok::Negation),
            '~' => Some(Tok::BitwiseComplement),
            '!' => Some(Tok::LogicalNegation),
            '+' => Some(Tok::Addition),
            '*' => Some(Tok::Multiplication),
            '/' => Some(Tok::Division),
            '<' => Some(Tok::LessThan),
            '>' => Some(Tok::GreaterThan),
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

fn word_to_tok(word: String) -> Result<Tok, LexError> {
    match word.as_str() {
        "func" => return Ok(Tok::Func),
        "int" => return Ok(Tok::Int),
        "return" => return Ok(Tok::Return),
        _ => {}
    }

    if word.chars().nth(0).unwrap().is_numeric() {
        let mut base = 10;
        let mut s = word.as_str();
        if let Some(nov_s) = s.strip_prefix("0x") {
            base = 16;
            s = nov_s;
        }
        else if let Some(nov_s) = s.strip_prefix("0b") {
            base = 2;
            s = nov_s;
        }
        let s = s.replace('_', "");
        match i32::from_str_radix(&s, base) {
            Ok(val) => return Ok(Tok::IntLiteral(val)),
            Err(e) => match e.kind() {
                IntErrorKind::InvalidDigit => Err(LexError {
                    kind: LexErrorKind::InvalidDigit,
                    word,
                }),
                _ => panic!("error: {:?}", e),
            },
        }
    }
    else {
        if word.chars().all(|c| c.is_ascii_alphabetic() || c.is_ascii_digit()) {
            return Ok(Tok::Identifier(word));
        } else {
            return Err(LexError {
                kind: LexErrorKind::InvalidCharacter,
                word,
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
            Tok::CloseBrace,
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
            Tok::CloseBrace,
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
        assert_eq!(
            result,
            Err(LexError {
                kind: LexErrorKind::InvalidDigit,
                word: "2a".to_string()
            })
        );
    }

    #[test]
    fn test_int_literals() {
        let line = "{19 0x12 3 0b01001001 0b1010_1100}";
        let expect: Vec<Tok> = vec![
            Tok::OpenBrace,
            Tok::IntLiteral(19),
            Tok::IntLiteral(18),
            Tok::IntLiteral(3),
            Tok::IntLiteral(73),
            Tok::IntLiteral(172),
            Tok::CloseBrace,
        ];
        let mut lex = Vec::new();
        let result = lex_line(line, &mut lex);
        assert_eq!(result, Ok(()));
        assert_eq!(lex, expect);
    }

    #[test]
    fn test_negations() {
        let line = "{-51 11 ~14 !1";
        let expect: Vec<Tok> = vec![
            Tok::OpenBrace,
            Tok::Negation,
            Tok::IntLiteral(51),
            Tok::IntLiteral(11),
            Tok::BitwiseComplement,
            Tok::IntLiteral(14),
            Tok::LogicalNegation,
            Tok::IntLiteral(1),
        ];
        let mut lex = Vec::new();
        let result = lex_line(line, &mut lex);
        assert_eq!(result, Ok(()));
        assert_eq!(lex, expect);
    }

    #[test]
    fn test_binary_ops() {
        let line = "5 +5 *1- 2 / 9";
        let expect: Vec<Tok> = vec![
            Tok::IntLiteral(5),
            Tok::Addition,
            Tok::IntLiteral(5),
            Tok::Multiplication,
            Tok::IntLiteral(1),
            Tok::Negation,
            Tok::IntLiteral(2),
            Tok::Division,
            Tok::IntLiteral(9),
        ];
        let mut lex = Vec::new();
        let result = lex_line(line, &mut lex);
        assert_eq!(result, Ok(()));
        assert_eq!(lex, expect);
    }

    #[test]
    fn test_two_chars() {
        let line = "test==abc|| 2 &&!= < <= > >=";
        let expect: Vec<Tok> = vec![
            Tok::Identifier("test".to_string()),
            Tok::Equal,
            Tok::Identifier("abc".to_string()),
            Tok::Or,
            Tok::IntLiteral(2),
            Tok::And,
            Tok::NotEqual,
            Tok::LessThan,
            Tok::LessThanOrEqual,
            Tok::GreaterThan,
            Tok::GreaterThanOrEqual,
        ];
        let mut lex = Vec::new();
        let result = lex_line(line, &mut lex);
        assert_eq!(result, Ok(()));
        assert_eq!(lex, expect);
    }
}
