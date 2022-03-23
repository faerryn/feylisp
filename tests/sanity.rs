#[cfg(test)]
mod tests {
    use feylisp::{eval::Environment, eval_src, expr::Expression};
    #[test]
    fn factorial() {
        let src = "
(let ((Z (lambda (r) ((lambda (f) (f f)) (lambda (f) (r (lambda (x) ((f f) x)))))))
      (fact-partial (lambda (f) (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))))
  ((Z fact-partial) 5))
";
        let env = Environment::standard_env();
        let (exprs, _) = eval_src(src, env);
        assert!(matches!(exprs.as_slice(), [Expression::Number(120)]));
    }

    #[test]
    fn fibonnaci() {
        let src = "
(define Z (lambda (r) ((lambda (f) (f f)) (lambda (f) (r (lambda (x) ((f f) x)))))))
(define fib (Z (lambda (f) (lambda (n) (if (< n 2) n (+ (f (- n 1)) (f (- n 2))))))))
(fib 10)
";
        let env = Environment::standard_env();
        let (exprs, _) = eval_src(src, env);
        assert!(matches!(exprs.as_slice(), [Expression::Number(55)]));
    }
}
