use crate::lex::Lexeme;
use crate::expr::Expression;

#[derive(Debug)]
pub struct Error;

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "parse error")
    }
}

impl std::error::Error for Error {}

pub fn parse(src: Vec<Lexeme>) -> Result<Vec<Expression>, Error> {
    let mut stack = vec![vec![]];

    for lexeme in src {
        match lexeme {
            Lexeme::Open => {
                stack.push(vec![]);
            }
            Lexeme::Close => {
                let list = stack.pop().ok_or(Error)?.into();
                stack
                    .last_mut()
                    .ok_or(Error)?
                    .push(Expression::List(list));
            }
            Lexeme::Number(number) => {
                stack
                    .last_mut()
                    .ok_or(Error)?
                    .push(Expression::Number(number));
            }
            Lexeme::Symbol(symbol) => {
                stack
                    .last_mut()
                    .ok_or(Error)?
                    .push(match symbol.as_str() {
                        "#t" => Expression::Bool(true),
                        "#f" => Expression::Bool(false),
                        _ => Expression::Symbol(symbol),
                    });
            }
        }
    }

    match &mut stack[..] {
        [top] => Ok(std::mem::take(top)),
        _ => Err(Error),
    }
}
