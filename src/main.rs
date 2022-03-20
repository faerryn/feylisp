use feylisp::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut env = Environment::default();

    let mut want_repl = false;
    let mut loaded_files = false;

    for arg in std::env::args().skip(1) {
        if arg == "--repl" {
            want_repl = true;
        } else {
            let src = std::fs::read_to_string(arg)?;
            let (exprs, new_env) = pipeline(&src, env)?;
            for expr in exprs {
                println!("{}", expr);
            }
            env = new_env;
            loaded_files = true;
        }
    }

    if want_repl || !loaded_files {
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
      (fact (lambda (f) (lambda (n) (if (zero n) 1 (mul n (f (sub n 1))))))))
  ((Y fact) 5))
";
        let env = Environment::default();
        let result = pipeline(src, env);
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
(define fib (lambda (f) (lambda (n) (if (lt n 2) n (sum (f (sub n 1)) (f (sub n 2)))))))
((Y fib) 10)
";
        let env = Environment::default();
        let result = pipeline(src, env);
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
enum PipelineError {
    Lex(LexError),
    Parse(ParseError),
    Eval(EvalError),
}

impl std::fmt::Display for PipelineError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for PipelineError {}

fn pipeline(
    src: &str,
    mut env: Environment,
) -> Result<(Vec<Expression>, Environment), PipelineError> {
    let exprs = parse(lex(src).map_err(PipelineError::Lex)?).map_err(PipelineError::Parse)?;
    let mut result = vec![];
    result.reserve(exprs.len());

    for expr in exprs {
        let (expr, new_env) = eval(expr, env).map_err(PipelineError::Eval)?;
        if let Some(expr) = expr {
            result.push(expr);
        }
        env = new_env;
    }

    Ok((result, env))
}
