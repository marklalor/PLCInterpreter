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
