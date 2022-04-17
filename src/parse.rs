use crate::{expr::Builtin, expr::Expression, lex::Lexeme};
use std::rc::Rc;

#[derive(Debug)]
pub enum Error {
    UnexpectedClose,
    UnclosedList,
    UnclosedQuote,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for Error {}

fn parse_helper<I: Iterator<Item = Lexeme>>(iter: &mut I) -> Result<Option<Expression>, Error> {
    if let Some(lexeme) = iter.next() {
        match lexeme {
            Lexeme::Open => {
                let mut list = vec![];
                loop {
                    match parse_helper(iter) {
                        Ok(Some(elt)) => list.push(elt),
                        Err(Error::UnexpectedClose) => break,
                        Ok(None) => return Err(Error::UnclosedList),
                        Err(err) => return Err(err),
                    }
                }
                Ok(Some(Expression::List(Rc::new(list.into_iter().collect()))))
            }
            Lexeme::Close => Err(Error::UnexpectedClose),
            Lexeme::Number(number) => Ok(Some(Expression::Number(number))),
            Lexeme::Symbol(symbol) => Ok(Some(match symbol.as_str() {
                "#t" => Expression::Bool(true),
                "#f" => Expression::Bool(false),
                _ => Expression::Symbol(Rc::new(symbol)),
            })),
            Lexeme::Quote => {
                if let Some(elt) = parse_helper(iter)? {
                    Ok(Some(Expression::List(Rc::new(
                        vec![Expression::Builtin(Builtin::Quote), elt]
                            .into_iter()
                            .collect(),
                    ))))
                } else {
                    Err(Error::UnclosedQuote)
                }
            }
        }
    } else {
        Ok(None)
    }
}

pub fn parse(src: Vec<Lexeme>) -> Result<Vec<Expression>, Error> {
    let mut result = vec![];
    let mut iter = src.into_iter();
    while let Some(expr) = parse_helper(&mut iter)? {
        result.push(expr)
    }
    Ok(result)
}
