; just a memoized fibbonacci machine, nothing to see here
(let mem (list 0 1))
(let fib
  (func (n)
	(if (< n (len mem))
	  (at mem n)
	  (push mem
		(+ (fib (+ n -1)) (fib (+ n -2)))))))
(print "fib(70) = " (fib 70) "\n")
