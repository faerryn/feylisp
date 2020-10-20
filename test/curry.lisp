; this kind of stuff needs closures!
(let add (func (a) () (func (b) (a) (let c (+ a b)) (func () (c) c))))
