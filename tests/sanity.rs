#[cfg(test)]
mod tests {
    use feylisp::{eval_src, expr::{Expression, Environment}};
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
(define fib (Y (lambda (f) (lambda (n) (if (< n 2) n (+ (f (- n 1)) (f (- n 2))))))))
(fib 10)
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
