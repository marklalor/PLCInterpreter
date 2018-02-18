#!/usr/bin/racket
#lang racket

(require "abstractions.scm")
(provide (all-defined-out))

; remove the first variable and value from the state
(define cdrstate
  (lambda (s)
    (cons (cdr (var-list s)) (cons (cdr (value-list s)) '()))))

; 
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

; statement checks
(define assignment?
  (lambda (stmt)
    (and (list? stmt) (eq? '= (operator stmt)))))

(define declaration?
  (lambda (stmt)
    (and (null? (cddr stmt)) (eq? (car stmt) 'var))))

(define declarationandassignment?
  (lambda (stmt)
    (and (not (null? (cddr stmt))) (eq? (car stmt) 'var))))

(define return?
  (lambda (stmt)
    (eq? 'return (car stmt))))

(define while?
  (lambda (stmt)
    (eq? 'while (car stmt))))

; negative vs substraction 
(define negative?
  (lambda (exp)
    (and (eq? (operator exp) '-)
         (null? (cddr exp)))))

(define subtraction?
  (lambda (exp)
    (and (eq? (operator exp) '-)
         (not (null? (cddr exp))))))

; error checks 

(define declared?
  (lambda (var s)
    (cond
      ((null? (var-list s)) #f)
      ((eq? (carvar s) var) #t)
      (else (declared? var (cdrstate s))))))

; takes a variable and a state, returns #t if the value of variable is not 'error else #f
(define assigned?
  (lambda (variable s)
    (cond
      ((not (declared? variable s)) #f)
      ((eq? (carvar s) variable)
       (not (eq? (carval s) 'error)))
      (else
       (assigned? variable (cdrstate s))))))


; our wrappers for bool operations
; our and operator, takes two atoms of 'true or 'false
(define myand
  (lambda (bool1 bool2)
    (and (atomtobool bool1) (atomtobool bool2)))) ; convert atom 'true or 'false to #t or #f else run into weird bugs 

; our or operator, takes two atoms of 'true or 'false
(define myor
  (lambda (bool1 bool2)
    (or (atomtobool bool1) (atomtobool bool2))))

; our not operator, takes an atom of 'true or 'false
(define mynot
  (lambda (bool)
    (not (atomtobool bool))))

(define booloperator
  (lambda (op)
    (lambda (bool1 bool2)
      (op (atomtobool bool1) (atomtobool bool2)))))

(define atomtobool
  (lambda (atom)
    (cond
      ((eq? atom 'true) #t)
      ((eq? atom 'false) #f)
      (else (error 'InvalidBool)))))

(define noteq
 (lambda (bool1 bool2)
   (not (eq? bool1 bool2))))

(define booltuple-to-atomtuple
  (lambda (booltuple)
    (if (m-value-exp-value booltuple)
      (2tuple 'true (m-value-exp-state booltuple))
      (2tuple 'false (m-value-exp-state booltuple)))))

(define add-var
  (lambda (variable value s)
    (cons (cons variable (var-list s)) (cons (cons value (value-list s)) '()))))

(define remove-var
  (lambda (variable s)
    (cond
      ((null? (var-list s)) s)
      ((eq? (car (var-list s)) variable) (cdrstate s))
      (else
       (add-var (car (var-list s)) (car (value-list s)) (remove-var variable (cdrstate s)))))))

; takes a value and a state, creates a tuple of the value and state
(define 2tuple
  (lambda (value s)
    (cons value (cons s '()))))
  