(let mem (list 0 1))
(let fib
  (func (n)
	(if (< n (len mem))
	  (at mem n)
	  (push mem
		(+ (fib (+ n -1)) (fib (+ n -2)))))))
