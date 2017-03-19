#lang racket/base

(require (for-syntax racket/base))

(define-syntax (ME-module-begin stx)
  (syntax-case stx ()
    [(_ s-expressions ...)
     #'(#%module-begin (apply values
                              (for/list ([expr (list 's-expressions ...)]
                                         #:when (not (eq? (m-eval expr
                                                                  global-environment)
                                                          'assert-success)))
                                (m-eval expr global-environment)))
                       )]))

(provide (rename-out (ME-module-begin #%module-begin))
         #%top-interaction)

(define (m-eval exp env)
  (cond [(assert? exp) (assert exp env)]
        [(self-evaluating? exp) exp]
        [(identifier? exp) (look-up exp env)]
        [(quote? exp) (remove-quote exp)]
        [(definition? exp) (m-eval-definition exp env)]
        [(lambda? exp) (m-eval-lambda exp env)]
        [(application? exp) (m-apply (m-eval (operator exp) env)
                                     (arg-list exp))]
        [else (format "Unknown expression ~a" exp)])
  )

(define (m-apply operator arg-list)
  (apply operator arg-list ))

(define (assert? exp)
  (tagged-with? 'assert exp))

(define (assert exp env)
  (if (eq? (m-eval (to-be-evaluated exp) env) (expected-result exp))
      'assert-success
      (format "assert failed with ~a" exp)))

(define (quote? exp)
  (tagged-with? 'quote exp))

(define (remove-quote exp)
  (quoted exp))

(define (definition? exp)
  (tagged-with? 'define exp))

(define (m-eval-definition exp env)
  (add-to-environment (define-var exp) (define-val exp) env))

(define (lambda? exp)
  (tagged-with? 'lambda exp))
(define (m-eval-lambda exp env)
  (make-procedure exp env))

(define (procedure? exp)
  (tagged-with? 'procedure exp))

; --- Implementation ---

(define (tagged-with? tag exp)
  (and (pair? exp)
       (eq? (car exp) tag)))

(define to-be-evaluated cadr)
(define expected-result caddr)

(define (self-evaluating? exp)
  (or (number? exp) (string? exp)))

(define identifier? symbol?)

(define (look-up var env)
  (if (no-more-frames? env)
      (format "No binding for ~a\n" var)
      (if (in-current-frame? var env)
          (look-up-in-current-frame var env)
          (look-up var (encompassing-env env)))))

(define quoted cadr)

(define define-var cadr)
(define define-val caddr)

(define (make-procedure exp env)
  (list 'procedure (procedure-variables exp) (procedure-body exp) env))

(define procedure-variables cadr)
(define procedure-body caddr)

(define application? pair?)
(define operator car)
(define arg-list cdr)

(define make-new-frame make-hash)
(define (add-to-environment var val env)
  (hash-set! (bottom-frame env) var val))
(define environment-with-no-frames '())
(define empty-frame '())
(define (drop-frame env)
  (cons (make-new-frame) env))
(define bottom-frame car)

(define (no-more-frames? env) (null? env))
(define (in-current-frame? var env) (hash-has-key? (current-frame env) var))
(define (look-up-in-current-frame var env) (hash-ref (current-frame env) var))
(define (encompassing-env env) (cdr env))
(define (current-frame env) (car env))

; --- Action ---

(define global-environment (drop-frame environment-with-no-frames))
(add-to-environment '+ + global-environment)