#[cfg(test)]
mod tests {
    use feylisp::{
        eval_src,
        expr::{Callable, Expression},
        standard_env,
    };
    #[test]
    fn let_binding_factorial() {
        let (exprs, _) = eval_src(
            "
(let ((fact-partial (lambda (f) (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))))
  (let ((fact (Z fact-partial)))
    (fact 5)))
",
            standard_env(),
        )
        .unwrap();
        assert_eq!(exprs.len(), 1);
        assert!(matches!(*exprs[0], Expression::Number(120)));
    }

    #[test]
    fn define_fibonnaci() {
        let (exprs, _) = eval_src(
            "
(define fib (Z (lambda (f) (lambda (n) (if (< n 2) n (+ (f (- n 1)) (f (- n 2))))))))
(fib 10)
",
            standard_env(),
        )
        .unwrap();
        assert_eq!(exprs.len(), 2);
        assert!(matches!(
            exprs[0].as_ref(),
            Expression::Callable(callable) if matches!(callable.as_ref(), Callable::Closure(_))
        ));
        assert!(matches!(*exprs[1], Expression::Number(55)));
    }

    #[test]
    fn varargs_map() {
        let (exprs, _) = eval_src(
            "
(map + '(1 2 3) '(4 5 6))
",
            standard_env(),
        )
        .unwrap();
        assert!(exprs.len() == 1);
        assert!(matches!(*exprs[0], Expression::List(_)));
        if let Expression::List(list) = &*exprs[0] {
            let list: Vec<_> = list.as_ref().into_iter().collect::<_>();
            assert_eq!(list.len(), 3);
            assert!(matches!(*list[0], Expression::Number(5)));
            assert!(matches!(*list[1], Expression::Number(7)));
            assert!(matches!(*list[2], Expression::Number(9)));
        } else {
            unreachable!();
        }
    }
}
