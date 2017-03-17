#lang racket/base

(require (for-syntax racket/base))

(define-syntax (ME-module-begin stx)
  (syntax-case stx ()
    [(_ s-expressions ...)
     #'(#%module-begin (apply values
                              (for/list ([expr (list 's-expressions ...)]
                                         #:when (not (eq? (m-eval expr) 'assert-success)))
                                (m-eval expr)))
                       )]))

(provide (rename-out (ME-module-begin #%module-begin))
         #%top-interaction)

(define (m-eval exp)
  (cond [(assert? exp) (assert exp)]
        [(self-evaluating? exp) exp]
        [(identifier? exp) (look-up exp)]
        [(quote? exp) (remove-quote exp)]
        [(definition? exp) (m-eval-definition exp)]
        [(lambda? exp) (m-eval-lambda exp)]
        [(application? exp) (m-apply exp)]
        [else (format "Unknown expression ~a" exp)])
  )

(define (m-apply exp)
  (apply (m-eval (operator exp)) (arg-list exp)))

(define (tagged-with? tag exp)
  (and (pair? exp)
       (eq? (car exp) tag)))

(define (assert? exp)
  (tagged-with? 'assert exp))
(define to-be-evaluated cadr)
(define expected-result caddr)

(define (assert exp)
  (if (eq? (m-eval (to-be-evaluated exp)) (expected-result exp))
      'assert-success
      (format "assert failed with ~a" exp)))

(define (self-evaluating? exp)
  (or (number? exp) (string? exp)))

(define identifier? symbol?)
(define (look-up exp)
  (hash-ref global-environment exp))

(define (quote? exp)
  (tagged-with? 'quote exp))
(define quoted cadr)
(define (remove-quote exp)
  (quoted exp))

(define (definition? exp)
  (tagged-with? 'define exp))
(define define-var cadr)
(define define-val caddr)
(define (m-eval-definition exp)
  (add-to-environment (define-var exp) (define-val exp) global-environment))

(define (lambda? exp)
  (tagged-with? 'lambda exp))
(define (m-eval-lambda exp)
  (make-procedure exp global-environment))
(define (make-procedure exp env)
  (list 'procedure (procedure-variables exp) (procedure-body exp) env))

(define (procedure? exp)
  (tagged-with? 'procedure exp))
(define procedure-variables cadr)
(define procedure-body caddr)

(define application? pair?)
(define operator car)
(define arg-list cdr)

(define make-new-frame make-hash)
(define (add-to-environment var val env)
  (hash-set! env var val))

(define global-environment (make-new-frame (list (cons '+ +))))

