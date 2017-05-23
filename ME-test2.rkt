(module ME-test "ME-expander.rkt"
  (#%module-begin
   (apply
    values
    (for/list
     ([expr
       (list
        '(assert 4 4)
        '(assert "abc" "abc")
        '(assert (+ 20 22) 42)
        '(assert 'literal literal)
        '(define x 3)
        '(assert x 3)
        '(assert ((lambda (x) (+ 2 x)) 50) 52))]
      #:when
      (not (eq? (m-eval expr global-environment) 'assert-success)))
     (m-eval expr global-environment)))))