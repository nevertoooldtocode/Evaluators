#lang s-exp "ME-expander.rkt"

(assert 4 4)
(assert "abc" "abc")
(assert (+ 20 22) 42)
(assert 'literal literal)

(define x 3)
(assert x 3)

(lambda (x) x)


(assert (lambda (x) (* 2 x)) 4)