#lang racket/base

; --- DSL ---

(require (for-syntax racket/base))
(provide (rename-out (ME-module-begin #%module-begin))
         #%top-interaction)

(define-syntax (ME-module-begin stx)
  (syntax-case stx ()
    [(_ s-expressions ...)
     #'(#%module-begin (apply values
                              (for/list ([expr (list 's-expressions ...)])
                                (m-eval expr global-environment)))
                       )]))

(define-syntax (#%top-interaction stx)
;  (printf "top-interaction with ~a\n" stx)
  #`(m-eval '#,(cdr (syntax-e stx)) global-environment))

; --- Begin of Evaluator ---

(define (m-eval exp env)
  ;    (printf "m-eval with ~a and ~a\n" exp env)
  ;    (printf "m-eval with ~a\n" exp)
  (cond [(assert? exp) (assert exp env)]
        [(self-evaluating? exp) exp]
        [(identifier? exp) (look-up exp env)]
        [(quote? exp) (quoted-exp exp)]
        [(definition? exp) (m-eval-definition exp env)]
        [(assignment? exp) (m-eval-assignment exp env)]
        [(begin? exp) (m-eval-sequence (begin-actions exp) env)]
        [(if? exp) (m-eval-if exp env)]
        [(cond? exp) (m-eval (cond->if exp) env)]
        [(lambda? exp) (m-eval-lambda exp env)]
        [(let? exp) (m-eval (let->lambda exp) env)]
        [(let*? exp) (m-eval (let*->nested-lets exp) env)]
        [(application? exp) (m-apply (m-eval (application-operator exp) env)
                                     (evaluated-arg-list exp env))]
        [else (eprintf "--- m-eval --- Unknown expression ~a" exp)])
  )

(define (m-apply procedure arg-list)
  (cond [(primitive-procedure? procedure) (apply-primitive (primitive-operator procedure) arg-list)]
        [(compound-procedure? procedure) 
           (m-eval-sequence (procedure-body procedure)
                            (extend-env (procedure-variables procedure)
                                        arg-list
                                        (procedure-environment procedure)))]
        [else (eprintf "--- m-apply --- Unknown operator ~a" procedure)]))

(define (m-eval-sequence body env)
  (for/last ([exp body])
    (m-eval exp env)))

(define (m-eval-if if-exp env)
  (if (m-eval (predicate if-exp) env)
      (m-eval (consequent if-exp) env)
      (m-eval (alternative if-exp) env)))

(define (look-up var env)
  (if (no-more-frames? env)
      (eprintf "--- look-up --- No binding for ~a" var)
      (if (in-current-frame? var env)
          (look-up-in-current-frame var env)
          (look-up var (encompassing-env env)))))

(define (m-eval-definition exp env)
  (define-variable! (define-var exp)
                       (m-eval (define-val exp) env)
                       env))

(define (m-eval-assignment exp env)
  (assign-variable! (assignment-var exp)
                       (m-eval (assignment-val exp) env)
                       env))

(define (m-eval-lambda exp env)
  (make-procedure exp env))

(define (self-evaluating? exp)
  (or (number? exp) (string? exp)))

(define (evaluated-arg-list exp env)
  (for/list ([arg (application-arg-list exp)])
    (m-eval arg env)))

(define (in-environment? var env)
  (if (no-more-frames? env)
      #f
      (if (in-current-frame? var env)
          #t
          (in-environment? var (encompassing-env env)))))

; --- manipulate S-expressions without evaluating ---

(define (cond->if cond-exp)
  (expand-clauses (cond-clauses cond-exp)))

(define (expand-clauses clauses)  
  (let* ([reversed-clauses (reverse clauses)]
         [last-clause (car reversed-clauses)]
         [remaining-clauses (cdr reversed-clauses)])
    (for/fold ([nested-ifs (if (cond-else-clause? last-clause)
                               (sequence->expression (cdr last-clause))
                               '(void))])
              ([exp reversed-clauses] #:when (not (cond-else-clause? exp)))
      (list 'if (car exp) (sequence->expression (cdr exp))  nested-ifs ))))

(define (expand-clauses2 clauses)
    (if (null? clauses)
        '(void)
        (if (cond-else-clause? (car clauses))
            (sequence->expression (cdar clauses))
            (list 'if
                  (caar clauses)
                  (sequence->expression (cdar clauses))
                  (expand-clauses2 (cdr clauses))))))

(define (let->lambda let-exp)
  (let* ([var-arg-list (let-var-arg-list let-exp)]
         [vars (map car var-arg-list)]
         [args (map cadr var-arg-list)]
         [body (let-body let-exp)])
    (cons (cons 'lambda (cons vars body)) args)))

(define (let*->nested-lets let*-exp)
  (helper (let*-var-arg-list let*-exp) (let*-body let*-exp)))

(define (helper var-arg-list body)
    (if (null? (cdr var-arg-list))
        (make-last-let var-arg-list body)
        (make-let (list (car var-arg-list)) (helper (cdr var-arg-list) body))))

; --- Predicates ---

(define (tagged-with? tag exp)
  (and (pair? exp)
       (eq? (car exp) tag)))

(define (identifier? exp) (symbol? exp))

(define (quote? exp)
  (tagged-with? 'quote exp))

(define (definition? exp)
  (tagged-with? 'define exp))

(define (assignment? exp)
  (tagged-with? 'set! exp))

(define (lambda? exp)
  (tagged-with? 'lambda exp))

(define (primitive-procedure? exp)
  (tagged-with? 'primitive exp))

(define (compound-procedure? exp)
  (tagged-with? 'procedure exp))

(define (begin? exp)
  (tagged-with? 'begin exp))

(define (if? exp)
  (tagged-with? 'if exp))

(define (cond? exp)
  (tagged-with? 'cond exp))

(define (cond-else-clause? exp)
  (tagged-with? 'else exp))

(define (let? exp)
  (tagged-with? 'let exp))

(define (let*? exp)
  (tagged-with? 'let* exp))

; --- Assert for testing ---

(define (assert? exp)
  (tagged-with? 'assert exp))

(define (assert exp env)
  (let* ([to-be-evaluated (assert-to-be-evaluated exp)]
         [result (m-eval to-be-evaluated env)]
         [expected-result (assert-expected-result exp)])
  (if (eq? result expected-result)
      (void)
      (eprintf
       "assert failed!/nExpression evaluated: ~a\nEvaluated result: ~a\nExpected result: ~a\n\n"
       to-be-evaluated
       result
       expected-result))))

(define assert-to-be-evaluated cadr)
(define assert-expected-result caddr)

; --- Implementation ---

(define apply-primitive apply)

(define quoted-exp cadr)

(define define-var cadr)
(define define-val caddr)

(define assignment-var cadr)
(define assignment-val caddr)

(define (make-procedure lambda-exp env)
  (list 'procedure (lambda-variables lambda-exp) (lambda-body lambda-exp) env))

(define (lambda-variables lambda-exp) (cadr lambda-exp))
(define (lambda-body lambda-exp) (cddr lambda-exp))

(define (procedure-variables procedure) (cadr procedure))
(define (procedure-body procedure) (caddr procedure))
(define (procedure-environment procedure) (cadddr procedure))

(define application? pair?)
(define application-operator car)
(define application-arg-list cdr)

(define (primitive-operator operator) (cadr operator))

(define (define-variable! var val env)
  (add-to-environment! var val env))

(define (assign-variable! var val env)
  (if (in-environment? var env)
      (add-to-environment! var val env)
      (eprintf "--- assignment --- No binding for ~a" var)))

(define (make-new-frame vars vals)
  (make-hash (for/list ([var vars][val vals]) (cons var val))))
(define (add-to-environment! var val env)
  (hash-set! (current-frame env) var val))
(define empty-environment '())
(define empty-frame '())
(define (extend-env vars vals base-env)
  (cons (make-new-frame vars vals) base-env))

(define (no-more-frames? env) (null? env))
(define (in-current-frame? var env) (hash-has-key? (current-frame env) var))
(define (look-up-in-current-frame var env) (hash-ref (current-frame env) var))
(define (encompassing-env env) (cdr env))
(define (current-frame env) (car env))

(define (sequence->expression seq)
  (cond [(null? seq) seq]
        [(last-exp? seq) (first-exp seq)]
        [else (make-begin seq)]))

(define (last-exp? sequence-of-expressions)
  (null? (cdr sequence-of-expressions)))
(define (first-exp sequence-of-expressions)
  (car sequence-of-expressions))

(define (make-begin sequence) (cons 'begin sequence))
(define (begin-actions begin-expr) (cdr begin-expr))

(define (predicate if-expr) (cadr if-expr))
(define (consequent if-expr) (caddr if-expr))
(define (alternative if-expr) (cadddr if-expr))

(define (cond-clauses cond-expr) (cdr cond-expr))

(define (let-body let-exp) (cddr let-exp))
(define (let-var-arg-list let-exp) (cadr let-exp))
(define (let*-body let-exp) (cddr let-exp))
(define (let*-var-arg-list let-exp) (cadr let-exp))
(define (make-let arg-val-list body) (list 'let arg-val-list body))
(define (make-last-let arg-val-list body) (cons 'let (cons arg-val-list body)))

; --- Set up Global Environment ---

(define global-environment (extend-env empty-environment '() '()))
(add-to-environment! '+ (list 'primitive +) global-environment)
(add-to-environment! '* (list 'primitive *) global-environment)
(add-to-environment! '- (list 'primitive -) global-environment)
(add-to-environment! '/ (list 'primitive /) global-environment)
(add-to-environment! 'eq? (list 'primitive eq?) global-environment)
(add-to-environment! 'void (list 'primitive void) global-environment)

; --- Test ---

(module+ test
  (require rackunit)
  (check-equal? (expand-clauses '([a b][c d]))
                '(if a b (if c d (void))))
  (check-equal? (expand-clauses '([a b1 b2][c d1 d2 d3 d4][else e1 e2 e3]))
                '(if a (begin b1 b2) (if c (begin d1 d2 d3 d4) (begin e1 e2 e3))))
  (check-equal? (expand-clauses2 '([a b][c d]))
                '(if a b (if c  d (void))))
  (check-equal? (expand-clauses2 '([a b1 b2][c d1 d2 d3 d4][else e1 e2 e3]))
                '(if a (begin b1 b2) (if c (begin d1 d2 d3 d4) (begin e1 e2 e3))))
  (m-eval '(begin (define x 3) (set! x 4)) global-environment)
  ;global-environment
  (check-equal? (let->lambda '(let ([x a] [y b]) b1 b2 b3))
                '((lambda (x y) b1 b2 b3) a b))
  (check-equal? (let*->nested-lets '(let* ([x a] [y b] [z c]) b1 b2 b3))
                '(let ([x a]) (let ([y b]) (let ([z c]) b1 b2 b3))))
  )

