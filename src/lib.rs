pub mod eval;
pub mod expr;
pub mod lex;
pub mod parse;

use crate::{
    eval::{eval, Environment},
    expr::Expression,
    lex::lex,
    parse::parse,
};

use std::rc::Rc;

use std::io::prelude::*;

#[must_use]
pub fn standard_env() -> Rc<Environment> {
    let env = Rc::new(Environment::core_env());

    let (_, env) = eval_src(
        "
(define car head)
(define cdr tail)

(define not (lambda (b) (if b #f #t)))
(define and (lambda (a b) (if a b a)))
(define or (lambda (a b) (if a a b)))

(define <= (lambda (a b) (or (= a b) (< a b))))
(define > (lambda (a b) (not (<= a b))))
(define >= (lambda (a b) (not (< a b))))

(define Z (lambda (r) ((lambda (f) (f f)) (lambda (f) (r (lambda (x) ((f f) x)))))))
(define foldr (lambda (f l v) ((Z (lambda (r) (lambda (l) (if (nil? l) v (f (head l) (r (tail l))))))) l)))
(define map (lambda (f l) (foldr (lambda (x acc) (cons (f x) acc)) l ())))
(define filter (lambda (f l) (foldr (lambda (x acc) (if (f x) (cons x acc) acc)) l ())))
",
        env,
    ).unwrap();

    env
}

#[derive(Debug)]
pub enum Error {
    IO(std::io::Error),
    Parse(parse::Error),
    Eval(eval::Error),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for Error {}

pub fn eval_src(
    src: &str,
    mut env: Rc<Environment>,
) -> Result<(Vec<Rc<Expression>>, Rc<Environment>), Error> {
    let exprs = parse(lex(src)).map_err(Error::Parse)?;
    let mut result = vec![];
    result.reserve(exprs.len());

    for expr in exprs {
        let (expr, new_env) = eval(Rc::new(expr), env).map_err(Error::Eval)?;
        if let Some(expr) = expr {
            result.push(expr);
        }
        env = new_env;
    }

    Ok((result, env))
}

pub fn repl(mut env: Rc<Environment>) -> Result<Rc<Environment>, std::io::Error> {
    let stdin = std::io::stdin();
    let mut stdout = std::io::stdout();

    let mut src = String::new();
    let mut line = String::new();

    print!("> ");
    stdout.flush()?;

    while stdin.read_line(&mut line)? > 0 {
        src.push_str(&line);
        line.clear();
        match parse(lex(&src)) {
            Ok(exprs) => {
                for expr in exprs {
                    match eval(Rc::new(expr), Rc::clone(&env)) {
                        Ok((expr, new_env)) => {
                            env = new_env;
                            if let Some(expr) = expr {
                                println!("{}", expr);
                            }
                        }
                        Err(err) => {
                            eprintln!("{}", err);
                        }
                    }
                }

                src.clear();
                print!("> ");
                stdout.flush()?;
            }
            Err(parse::Error::UnclosedList | parse::Error::UnclosedQuote) => {}
            Err(parse::Error::UnexpectedClose) => {
                src.clear();
                println!("unexpected ')'");
                print!("> ");
                stdout.flush()?;
            }
        }
    }

    Ok(env)
}
