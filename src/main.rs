use clap::Parser;
use feylisp::{eval, lex, parse, Environment, EvalError, Expression, LexError, ParseError};

#[derive(Debug, Parser)]
struct Args {
    files: Vec<String>,

    #[clap(short, long)]
    repl: bool,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    let mut env = Environment::default();
    for file in &args.files {
        let src = std::fs::read_to_string(file)?;
        let (exprs, new_env) = eval_src(&src, env)?;
        for expr in exprs {
            println!("{}", expr);
        }
        env = new_env;
    }

    if args.repl || args.files.is_empty() {
        env = repl(env)?;
        println!("[{}]", env);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn factorial() {
        let src = "
(let ((Y (lambda (r) ((lambda (f) (f f)) (lambda (f) (r (lambda (x) ((f f) x)))))))
      (fact (lambda (f) (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))))
  ((Y fact) 5))
";
        let env = Environment::default();
        let result = eval_src(src, env);
        let result = match result {
            Ok((ref exprs, env)) => Ok((exprs.as_slice(), env)),
            Err(err) => Err(err),
        };
        assert!(matches!(result, Ok(([Expression::Number(120)], _))));
    }

    #[test]
    fn fibonnaci() {
        let src = "
(define Y (lambda (r) ((lambda (f) (f f)) (lambda (f) (r (lambda (x) ((f f) x)))))))
(define fib (lambda (f) (lambda (n) (if (< n 2) n (+ (f (- n 1)) (f (- n 2)))))))
((Y fib) 10)
";
        let env = Environment::default();
        let result = eval_src(src, env);
        let result = match result {
            Ok((ref exprs, env)) => Ok((exprs.as_slice(), env)),
            Err(err) => Err(err),
        };
        assert!(matches!(result, Ok(([Expression::Number(55)], _))));
    }
}

fn repl(mut env: Environment) -> Result<Environment, Box<dyn std::error::Error>> {
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
        if let Some(exprs) = lex(&src).ok().and_then(|src| parse(src).ok()) {
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

#[derive(Debug)]
enum EvalSrcError {
    Lex(LexError),
    Parse(ParseError),
    Eval(EvalError),
}

impl std::fmt::Display for EvalSrcError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for EvalSrcError {}

fn eval_src(
    src: &str,
    mut env: Environment,
) -> Result<(Vec<Expression>, Environment), EvalSrcError> {
    let exprs = parse(lex(src).map_err(EvalSrcError::Lex)?).map_err(EvalSrcError::Parse)?;
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
