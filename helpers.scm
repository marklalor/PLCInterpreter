#!/usr/bin/racket
#lang racket

(require "abstractions.scm")
(provide (all-defined-out))

; remove the first variable and value from the state
(define cdrstate
  (lambda (s)
    (cons (cdr (var-list s)) (cons (cdr (value-list s)) '()))))

(define int-exp?
  (lambda (exp)
    (cond
      ((number? exp) #t)
      ((and (list? exp)
            (or
             (eq? (operator exp) '+)
             (eq? (operator exp) '-)
             (eq? (operator exp) '*)
             (eq? (operator exp) '/)
             (eq? (operator exp) '%)) #t))
      (else #f))))

(define bool-exp?
  (lambda (exp)
    (cond
      ((or (eq? exp 'true)
           (eq? exp 'false)) #t)
      ((and (list? exp)
            (or (eq? (operator exp) '!)
             (eq? (operator exp) '||)
             (eq? (operator exp) '&&)
             (eq? (operator exp) '>)
             (eq? (operator exp) '>=)
             (eq? (operator exp) '<)
             (eq? (operator exp) '<=)
             (eq? (operator exp) '==)
             (eq? (operator exp) '!=))) #t)
      (else #f))))

(define ifthenelse?
  (lambda (stmt)
    (cond
      ((null? stmt) #f)
      (else (and (eq? (operator stmt) 'if)
            (not (null? (cdddr stmt))))))))

(define ifthen?
  (lambda (stmt)
    (cond
      ((null? stmt) #f)
      (else (and (eq? (operator stmt) 'if)
            (null? (cdddr stmt)))))))
  