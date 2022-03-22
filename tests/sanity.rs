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
        let env = Environment::standard_env();
        let (exprs, _) = eval_src(src, env).unwrap();
        assert!(matches!(exprs.as_slice(), [Expression::Number(120)]));
    }

    #[test]
    fn fibonnaci() {
        let src = "
(define Y (lambda (r) ((lambda (f) (f f)) (lambda (f) (r (lambda (x) ((f f) x)))))))
(define fib (Y (lambda (f) (lambda (n) (if (< n 2) n (+ (f (- n 1)) (f (- n 2))))))))
(fib 10)
";
        let env = Environment::standard_env();
        let (exprs, _) = eval_src(src, env).unwrap();
        assert!(matches!(exprs.as_slice(), [Expression::Number(55)]));
    }
}
