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

use std::io::prelude::*;

#[must_use]
pub fn standard_env() -> Environment {
    let env = Environment::core_env();

    let (_, env) = eval_src(
        "
(define car head)
(define cdr tail)
(define not (lambda (b) (if b #f #t)))
(define and (macro (a b) (let ((a (eval a))) (cons 'quote (cons (if a (eval b) a) ())))))
(define or (macro (a b) (let ((a (eval a))) (cons 'quote (cons (if a a (eval b)) ())))))

(define Z (lambda (r) ((lambda (f) (f f)) (lambda (f) (r (lambda (x) ((f f) x)))))))
(define foldr (lambda (f v l) ((Z (lambda (r) (lambda (l) (if (nil? l) v (f (head l) (r (tail l))))))) l)))
(define map (lambda (f l) (foldr (lambda (x acc) (cons (f x) acc)) () l)))
(define filter (lambda (f l) (foldr (lambda (x acc) (if (f x) (cons x acc) acc)) () l)))
",
        env,
    );

    env
}

#[must_use]
pub fn eval_src(src: &str, mut env: Environment) -> (Vec<Expression>, Environment) {
    let exprs = parse(lex(src)).expect("parse fail");
    let mut result = vec![];
    result.reserve(exprs.len());

    for expr in exprs {
        let (expr, new_env) = eval(expr, env);
        if let Some(expr) = expr {
            result.push(expr);
        }
        env = new_env;
    }

    (result, env)
}

#[must_use]
pub fn repl(mut env: Environment) -> Environment {
    let stdin = std::io::stdin();
    let mut stdout = std::io::stdout();

    let mut src = String::new();
    let mut line = String::new();

    print!("> ");
    stdout.flush().expect("broken stdout");

    while stdin.read_line(&mut line).expect("broken stdin") > 0 {
        src.push_str(&line);
        line.clear();
        match parse(lex(&src)) {
            Ok(exprs) => {
                for expr in exprs {
                    let (expr, new_env) = eval(expr, env);
                    env = new_env;
                    if let Some(expr) = expr {
                        println!("{}", expr);
                    }
                }

                src.clear();
                print!("> ");
                stdout.flush().expect("broken stdout");
            }
            Err(parse::Error::UnclosedList) => {}
            Err(parse::Error::UnexpectedClose) => {
                src.clear();
                println!("unexpected ')'");
                print!("> ");
                stdout.flush().expect("broken stdout");
            }
            Err(parse::Error::UnclosedQuote) => {
                src.clear();
                println!("expected something after quote '");
                print!("> ");
                stdout.flush().expect("broken stdout");
            }
        }
    }

    env
}
