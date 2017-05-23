#lang s-exp "ME-expander-data-directed-analysis.rkt"

(assert 4 4)
(assert "abc" "abc")
(assert (+ 20 22) 42)
(assert (* (+ 2 3) (+ 3 4)) 35)
(assert 'literal literal)

(define x 3)
(assert x 3)
(define y (+ x 4))
(assert y 7)

(assert ((lambda (x) (+ 2 x)) 50) 52)
(assert ((lambda (x y z) (+ 2 x y z)) 50 40 30) 122)
(assert ((lambda (x y) (+ 1 y) (+ 2 x)) 50 60) 52)

(define square (lambda (x) (* x x)))
(assert (square 3) 9)

(assert (if (eq? (square 4) 16) 'istrue (/ 1 0)) istrue)
(assert (if (eq? (square 4) 17) (/ 1 0) 'isfalse) isfalse)
(assert (if true 'istrue (/ 1 0)) istrue)
(assert (if false (/ 1 0) 'isfalse) isfalse)

(assert (begin x) 3)
(assert (begin x y) 7)

(assert (cond [(eq? (square 4) 16) 'first]
              [(eq? (/ 1 0) 16) (/ 1 0)]
              [(eq? (/ 1 0) 16) (/ 1 0)]
              [else (/ 1 0)])
        first)

(assert (cond [(eq? (square 4) 17) (/ 1 0)]
              [(eq? (square 4) 16) 'statement1 'statement2 'second]
              [(eq? (/ 1 0) 16) (/ 1 0)]
              [else (/ 1 0)])
        second)

(assert (cond [(eq? (square 4) 18) (/ 1 0)]
              [(eq? (square 4) 17) (/ 1 0)]
              [(eq? (square 4) 16) 'statement1 'statement2 'third]
              [else (/ 1 0)])
        third)

(assert (cond [(eq? (square 4) 19) (/ 1 0)]
              [(eq? (square 4) 18) (/ 1 0)]
              [(eq? (square 4) 17) (/ 1 0)]
              [else 'statement1 'statement2 'fourth])
        fourth)

(cond [(eq? (square 4) 19) (/ 1 0)])

(set! x 4)
(assert x 4)

(assert (let ([x (+ 1 2)]
              [y 5])
          (set! x 4)
          (+ x y))
        9)

(assert (let ([x 2])
          (let ([y (+ x 3)])
            (+ x y)))
        7)

(assert (let* ([x 2]
               [y (+ x 3)])
          (+ x y))
        7)

(define factorial
  (lambda (n)
    (if (eq? n 1)
        1
        (* (factorial (- n 1)) n))))

(define for-each
  (lambda (proc seq)
    (if (null? seq)
        (void)
        (begin (proc (car seq))
               (for-each proc (cdr seq))))))

(assert (factorial 10) 3628800)

(for-each (lambda (x) 'x) '(a b c d))

(define quote-demo (lambda () 'y 'y 'y))
;(quote-demo)
;(quote-demo)
;(quote-demo)
;(quote-demo)
;(quote-demo)


