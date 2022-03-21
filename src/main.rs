use feylisp::{eval_src, repl, Environment};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut env = Environment::default();
    for file in std::env::args().skip(1) {
        let src = std::fs::read_to_string(file)?;
        let (exprs, new_env) = eval_src(&src, env)?;
        for expr in exprs {
            println!("{}", expr);
        }
        env = new_env;
    }

    env = repl(env)?;
    println!("[{}]", env);

    Ok(())
}

#[cfg(test)]
mod tests {
    use feylisp::{eval_src, Environment, Expression};

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
