#!/usr/bin/racket
#lang racket
(provide (all-defined-out))

; state abstractions

; get variable list of the state 
(define var-list car)

; get value list if the state 
(define value-list cadr)

; get first variable in variable list
(define carvar
  (lambda (s)
    (car (var-list s))))

; get first value in the variable list
(define carval
  (lambda (s)
    (car (value-list s))))

; statement abstractions

(define operator car)

; variable declaration: (var variable) or (var variable value)
(define declaration-variable cadr)
(define declaration-value caddr)

; assignment: (= variable expression)
(define assignment-variable cadr)
(define assignment-expression caddr)

; return: (return expression)
(define return-expression cadr)

; if statement (if conditional then-statement optional-else-statement)
(define if-condition cadr)
(define then-statement caddr)
(define else-statement cadddr)

; while statement: (while conditional body-statement)
(define while-condition cadr)
(define while-body-statement caddr)

; block statement 
(define block-stmt cadr)

; try catch finally
(define try-block cadr)
(define catch-block caddr)
(define finally-block
  (lambda (stmt)
    (cadr (cadddr stmt))))

(define catch-stmt caddr)
(define catch-exception
  (lambda (stmt)
    (car (cadr stmt))))

(define throw-exp cadr)

; Other expressions...

; +, -, *, /
(define binaryop-first cadr)
(define binaryop-second caddr)


(define operand1 cadr)
(define operand2 caddr)

; get the value of the tuple
(define m-value-exp-value car)

; get the state of the tuple
(define m-value-exp-state cadr)

; abstractions for layers in state
(define car-slayer
  (lambda (s)
    (car s)))

(define cdr-slayer
  (lambda (s)
    (cdr s)))

(define outer-slayer
  (lambda (s)
    (cadr s)))

(define global-slayer
  (lambda (s)
    (caddr s)))

(define init-state '((()())))

(define state '(()()))

(define return (lambda (v) v))

(define return-stmt cadr)

(define break?
  (lambda (stmt)
    (eq? (car stmt) 'break)))

; PART 3 ABSTRACTIONS

; abstractions for getting function name and closure from a statement 
(define func-name cadr)
(define param-body cddr)

; closure abstractions
(define formal-params
  (lambda (closure)
    (car (car closure))))

(define func-body
  (lambda (closure)
    (cadr (car closure))))

(define binding cadr)

; get the actual params for funcall
(define actual-params cddr)

(define main-function?
  (lambda (stmt)
    (cond 
    ((null? (cdr stmt)) #f)
    (else
     (and (eq? (car stmt) 'function)  (eq? (func-name stmt) 'main))))))




