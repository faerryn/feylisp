#[cfg(test)]
mod tests {
    use feylisp::{eval_src, expr::Expression, standard_env};
    #[test]
    fn factorial() {
        let (exprs, _) = eval_src(
            "
(let ((fact-partial (lambda (f) (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))))
(let ((fact (Z fact-partial)))
(fact 5)))
",
            standard_env(),
        )
        .unwrap();
        assert!(exprs.len() == 1);
        assert!(matches!(*exprs[0], Expression::Number(120)));
    }

    #[test]
    fn fibonnaci() {
        let (exprs, _) = eval_src(
            "
(define fib (Z (lambda (f) (lambda (n) (if (< n 2) n (+ (f (- n 1)) (f (- n 2))))))))
(fib 10)
",
            standard_env(),
        )
        .unwrap();
        assert!(exprs.len() == 1);
        assert!(matches!(*exprs[0], Expression::Number(55)));
    }

    #[test]
    fn foldr() {
        let (exprs, _) = eval_src(
            "
(map (lambda (x) (+ 1 x)) '(1 2 3))
",
            standard_env(),
        )
        .unwrap();
        assert!(exprs.len() == 1);
        assert!(matches!(*exprs[0], Expression::List(_)));
        if let Expression::List(list) = &*exprs[0] {
            let list: Vec<_> = list.into_iter().collect::<_>();
            assert!(matches!(
                list.as_slice(),
                [
                    Expression::Number(2),
                    Expression::Number(3),
                    Expression::Number(4),
                ]
            ));
        } else {
            unreachable!();
        }
    }
}
