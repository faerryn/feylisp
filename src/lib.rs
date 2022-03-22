pub mod eval;
pub mod expr;
pub mod lex;
pub mod parse;

use crate::eval::eval;
use crate::expr::{Environment, Expression};
use crate::lex::lex;
use crate::parse::parse;

#[derive(Debug)]
pub enum EvalSrcError {
    Parse(parse::Error),
    Eval(eval::Error),
}

impl std::fmt::Display for EvalSrcError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for EvalSrcError {}

pub fn eval_src(
    src: &str,
    mut env: Environment,
) -> Result<(Vec<Expression>, Environment), EvalSrcError> {
    let exprs = parse(lex(src)).map_err(EvalSrcError::Parse)?;
    let mut result = vec![];
    result.reserve(exprs.len());

    for expr in exprs {
        let (expr, new_env) = eval(expr, env).map_err(EvalSrcError::Eval)?;
        if let Some(expr) = expr {
            result.push(expr);
        }
        env = new_env;
    }

    Ok((result, env))
}

pub fn repl(mut env: Environment) -> Result<Environment, Box<dyn std::error::Error>> {
    use std::io::prelude::*;
    let stdin = std::io::stdin();
    let mut stdout = std::io::stdout();

    let mut src = String::new();
    let mut line = String::new();

    print!("> ");
    stdout.flush()?;

    while stdin.read_line(&mut line)? > 0 {
        src.push_str(&line);
        line.clear();
        if let Ok(exprs) = parse(lex(&src)) { // sketchy
            src.clear();
            for expr in exprs {
                let (expr, new_env) = eval(expr, env)?;
                env = new_env;
                if let Some(expr) = expr {
                    println!("{}", expr);
                }
            }
            print!("> ");
            stdout.flush()?;
        }
    }

    Ok(env)
}
