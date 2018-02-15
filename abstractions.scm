#!/usr/bin/racket
#lang racket
(provide (all-defined-out))

(define var-list
  (lambda (s)
    (car s)))

(define value-list
  (lambda (s)
    (cadr s)))

(define removefirstvar
  (lambda (s)
    (cons (cdr (var-list s)) (cons (cdr (value-list s)) '()))))


(define variable
  (lambda (stmt)
    (cadr stmt)))

(define expression
  (lambda (stmt)
    (caddr stmt)))

(define operator
  (lambda (exp)
    (car exp)))

(define operand1 cadr)
(define operand2 caddr)
