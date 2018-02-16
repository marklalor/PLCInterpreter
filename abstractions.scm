#!/usr/bin/racket
#lang racket
(provide (all-defined-out))

; state abstractions
(define var-list car)

(define value-list cadr)

(define carvar
  (lambda (s)
    (car (var-list s))))

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
(define while-conditional cadr)
(define while-body-statement caddr)

; Other expressions...

; +, -, *, /
(define binaryop-first cadr)
(define binaryop-second caddr)


(define operand1 cadr)
(define operand2 caddr)
