use crate::expr::Expression;
use crate::lex::Lexeme;

#[derive(Debug)]
pub enum Error {
    Unclosed,
    UnexpectedClose,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for Error {}

pub fn parse(src: Vec<Lexeme>) -> Result<Vec<Expression>, Error> {
    let mut stack = vec![];
    let mut top = vec![];

    for lexeme in src {
        match lexeme {
            Lexeme::Open => {
                stack.push(top);
                top = vec![];
            }
            Lexeme::Close => {
                let mut new_top = stack.pop().ok_or(Error::UnexpectedClose)?;
                new_top.push(Expression::List(top.into()));
                top = new_top;
            }
            Lexeme::Number(number) => {
                top.push(Expression::Number(number));
            }
            Lexeme::Symbol(symbol) => {
                top.push(match symbol.as_str() {
                    "#t" => Expression::Bool(true),
                    "#f" => Expression::Bool(false),
                    _ => Expression::Symbol(symbol),
                });
            }
        }
    }

    if stack.is_empty() {
        Ok(top)
    } else {
        Err(Error::Unclosed)
    }
}
