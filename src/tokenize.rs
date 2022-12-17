use crate::utils::Peeker;
use crate::vm::Stack;

#[derive(Debug)]
pub enum Operator {
    Plus,
    Minus,
    Mul,
}

#[derive(Debug)]
pub enum Token {
    Dump,
    At,
    Separator,
    String(String),
    Integer(isize),
    Float(f64),
    Identifier(String),
    Operator(Operator),
}

pub fn parse_into_instructions(tokens: &mut Stack<Token>) -> Stack<Stack<Token>> {
    let mut instructions = Stack::default();
    let mut current_instruction = Stack::default();

    while let Some(next) = tokens.pop() {
        match next {
            Token::Separator => {
                instructions.push(current_instruction);
                current_instruction = Stack::default()
            }
            _ => current_instruction.push(next),
        }
    }

    instructions.push(current_instruction);

    instructions
}

pub fn lex_into_tokens(code: &str) -> Stack<Token> {
    let mut chars = Peeker::new(code.chars().collect());
    let mut tokens = Stack::default();

    while let Some(char) = chars.next() {
        match char {
            ' ' | '\n' => {}
            ';' => tokens.push(Token::Separator),
            '@' => tokens.push(Token::At),
            '%' => tokens.push(Token::Dump),
            '*' => tokens.push(Token::Operator(Operator::Mul)),
            '+' => tokens.push(Token::Operator(Operator::Plus)),
            '-' => tokens.push(Token::Operator(Operator::Minus)),
            '\'' => {
                while let Some(next) = chars.next() {
                    if next == '\'' {
                        break;
                    }
                }
            }

            '"' => {
                let mut content = String::new();

                while let Some(next) = chars.next() {
                    if next == '"' {
                        break;
                    }

                    content.push(next)
                }

                tokens.push(Token::String(content))
            }
            'a'..='z' => {
                let mut content = String::from(char);

                while let Some(next) = chars.next() {
                    if ('a'..='z').contains(&next) {
                        content.push(next)
                    } else {
                        chars.previous();
                        break;
                    }
                }

                tokens.push(Token::Identifier(content))
            }
            '0'..='9' => {
                let mut content = String::from(char);
                let mut float = false;

                while let Some(next) = chars.next() {
                    if ('0'..='9').contains(&next) {
                        content.push(next)
                    } else if next == '.' {
                        content.push('.');
                        float = true
                    } else {
                        chars.previous();
                        break;
                    }
                }

                if float {
                    tokens.push(Token::Float(content.parse::<f64>().unwrap()))
                } else {
                    tokens.push(Token::Integer(content.parse::<isize>().unwrap()))
                }
            }
            _ => panic!(),
        }
    }

    tokens
}
