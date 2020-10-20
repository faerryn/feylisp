; this kind of stuff needs closures!
(let add (func (a) () (func (b) (a) (+ a b))))
(print ((add 1) 2) "\n")
