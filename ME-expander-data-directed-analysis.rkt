#lang racket/base

; --- DSL ---

(require (for-syntax racket/base
                     racket/syntax))
(provide (rename-out (ME-module-begin #%module-begin))
         #%top-interaction
         )

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

; --- Core of Evaluator ---

(define (m-eval exp env)
  ;    (printf "m-eval with ~a and ~a\n" exp env)
  ;(printf "m-eval with ~a\n" exp)
  ((analyze exp) env))

(define (analyze exp)
  ;(printf "analyze with ~a\n" exp)
  (cond [(self-evaluating? exp) (lambda (env) exp)]
        [(identifier? exp) (lambda (env) (look-up exp env))]
        [(special-form? exp) (analyze-special-form exp)]
        [(application? exp) (analyze-application exp)]
        [else (eprintf "--- analyze --- Unknown expression ~a" exp)])
  )

(define (analyze-application exp)
  (let ([a-operator (analyze (application-operator exp))]
        [a-arg-list (map analyze (application-arg-list exp))])
    (lambda (env)
      (execute-application (a-operator env)
                           (for/list ([a-arg a-arg-list]) (a-arg env))))))

(define (execute-application procedure arg-list)
  (cond [(primitive-procedure? procedure)
         (apply-primitive (primitive-operator procedure) arg-list)]
        [(compound-procedure? procedure) 
         ((procedure-body procedure)
          (extend-env (procedure-variables procedure)
                      arg-list
                      (procedure-environment procedure)))]
        [else (eprintf "--- execute-application --- Unknown operator ~a" procedure)]))

(define (analyze-sequence body)
  (let ([a-body (map analyze body)])
    (lambda (env)
;      (printf "an-seq2 xalled\n")
      (for/last ([a-exp a-body])
        (a-exp env)))))

(define (analyze-sequence1 exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
;    (printf "loop called\n")
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (loop (car procs) (cdr procs))))

(define (analyze-sequence2 exps)
  (define (execute-sequence procs env)
;    (printf "execute-seq called\n")
    (cond ((null? (cdr procs)) ((car procs) env))
          (else ((car procs) env)
                (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (lambda (env) (execute-sequence procs env))))

(define (look-up var env)
  (if (no-more-frames? env)
      (eprintf "--- look-up --- No binding for ~a" var)
      (if (in-current-frame? var env)
          (look-up-in-current-frame var env)
          (look-up var (encompassing-env env)))))

; --- Eval functions for Special Forms

(define (analyze-if if-exp)
  (let ([a-pred (analyze (predicate if-exp))]
        [a-cons (analyze (consequent if-exp))]
        [a-alt (analyze (alternative if-exp))])
    (lambda (env)
      (if (a-pred env)
          (a-cons env)
          (a-alt env)))))

(define (analyze-cond exp)
  (analyze (cond->if exp)))

(define (analyze-quote exp)
  (let ([qval (quoted-exp exp)])
    (lambda (env) qval)))

(define (analyze-definition exp)
  (let ([var (define-var exp)]
        [a-val (analyze (define-val exp))])
    (lambda (env)
      (define-variable! var
        (a-val env)
        env))))

(define (analyze-assignment exp)
  (let ([var (assignment-var exp)]
        [a-val (analyze (assignment-val exp))])
    (lambda (env)
      (assign-variable! var
                        (a-val env)
                        env))))

(define (analyze-begin exp)
  (analyze-sequence (begin-actions exp)))

(define (analyze-lambda exp)
  (let ([vars (lambda-variables exp)]
        [a-body (analyze-sequence (lambda-body exp))])
    (lambda (env)
      (make-procedure vars a-body env))))

(define (analyze-let exp)
  (analyze (let->lambda exp)))

(define (analyze-let* exp)
  (analyze (let*->nested-lets exp)))

(define (analyze-special-form exp)
  ((hash-ref analyze-table (car exp)) exp))

(define (analyze-assert exp)
  (lambda (env)
    (assert exp env)))

(define analyze-table
  (make-hash `((assert . ,analyze-assert)
               (quote . ,analyze-quote)
               (define . ,analyze-definition)
               (set! . ,analyze-assignment)
               (begin . ,analyze-begin)
               (if . ,analyze-if)
               (cond . ,analyze-cond)
               (lambda . ,analyze-lambda)
               (let . ,analyze-let)
               (let* . ,analyze-let*)
               )))


; --- manipulate S-expressions without evaluating ---

(define (cond->if cond-exp)
  (expand-clauses (cond-clauses cond-exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      '(void)
      (if (cond-else-clause? (car clauses))
          (sequence->expression (cdar clauses))
          (list 'if
                (caar clauses)
                (sequence->expression (cdar clauses))
                (expand-clauses (cdr clauses))))))

(define (sequence->expression seq)
  (cond [(null? seq) seq]
        [(last-exp? seq) (first-exp seq)]
        [else (make-begin seq)]))

(define (let->lambda let-exp)
  (let* ([var-arg-list (let-var-arg-list let-exp)]
         [vars (map car var-arg-list)]
         [args (map cadr var-arg-list)]
         [body (let-body let-exp)])
    (cons (cons 'lambda (cons vars body)) args)))

(define (let*->nested-lets let*-exp)
  (define (helper var-arg-list body)
    (if (null? (cdr var-arg-list))
        (make-last-let var-arg-list body)
        (make-let (list (car var-arg-list)) (helper (cdr var-arg-list) body))))
  (helper (let*-var-arg-list let*-exp) (let*-body let*-exp)))

; --- Predicates ---

(define (self-evaluating? exp)
  (or (number? exp) (string? exp)))

(define (identifier? exp) (symbol? exp))

(define (special-form? exp)
  (hash-has-key? analyze-table (car exp)))

(define application? pair?)

(define (primitive-procedure? exp)
  (tagged-with? 'primitive exp))

(define (compound-procedure? exp)
  (tagged-with? 'procedure exp))

(define (cond-else-clause? exp)
  (tagged-with? 'else exp))

(define (tagged-with? tag exp)
  (and (pair? exp)
       (eq? (car exp) tag)))

(define (in-environment? var env)
  (if (no-more-frames? env)
      #f
      (if (in-current-frame? var env)
          #t
          (in-environment? var (encompassing-env env)))))

; --- Assert for testing ---

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

(define (quoted-exp exp)
;  (printf "quoted-exp with ~a\n" exp)
  (cadr exp))

(define define-var cadr)
(define define-val caddr)

(define assignment-var cadr)
(define assignment-val caddr)

(define (make-procedure vars body env)
  (list 'procedure vars body env))

(define (lambda-variables lambda-exp) (cadr lambda-exp))
(define (lambda-body lambda-exp) (cddr lambda-exp))

(define (procedure-variables procedure) (cadr procedure))
(define (procedure-body procedure) (caddr procedure))
(define (procedure-environment procedure) (cadddr procedure))

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

(define (last-exp? sequence-of-expressions)
  (null? (cdr sequence-of-expressions)))
(define (first-exp sequence-of-expressions)
  (car sequence-of-expressions))

(define (make-begin sequence) (cons 'begin sequence))
(define (begin-actions begin-expr) (cdr begin-expr))

(define (predicate if-expr)
  ;  (printf "predicate-if called with ~a\n" if-expr)
  (cadr if-expr))
(define (consequent if-expr)
  ;  (printf "consequent-if called with ~a\n" if-expr)
  (caddr if-expr))
(define (alternative if-expr)
  ;  (printf "alternative-if called with ~a\n" if-expr)
  (cadddr if-expr))

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
(add-to-environment! 'car (list 'primitive car) global-environment)
(add-to-environment! 'cdr (list 'primitive cdr) global-environment)
(add-to-environment! 'null? (list 'primitive null?) global-environment)
(add-to-environment! 'true #t global-environment)
(add-to-environment! 'false #f global-environment)
(add-to-environment! 'displayln (list 'primitive displayln) global-environment)

; --- Test ---

(module+ test
  (require rackunit)
  (check-equal? (expand-clauses '([a b][c d]))
                '(if a b (if c d (void))))
  (check-equal? (expand-clauses '([a b1 b2][c d1 d2 d3 d4][else e1 e2 e3]))
                '(if a (begin b1 b2) (if c (begin d1 d2 d3 d4) (begin e1 e2 e3))))
  (m-eval '(begin (define x 3) (set! x 4)) global-environment)
  ;global-environment
  (check-equal? (let->lambda '(let ([x a] [y b]) b1 b2 b3))
                '((lambda (x y) b1 b2 b3) a b))
  (check-equal? (let*->nested-lets '(let* ([x a] [y b] [z c]) b1 b2 b3))
                '(let ([x a]) (let ([y b]) (let ([z c]) b1 b2 b3))))
  )

