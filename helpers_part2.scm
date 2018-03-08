#!/usr/bin/racket
#lang racket

(require "abstractions_part2.scm")
(provide (all-defined-out))

; remove the first variable and value from the state list
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

(define unary?
  (lambda (exp)
    (null? (cddr exp))))

(define declared-in-current-layer?
  (lambda (var slayer)
    (if (null? slayer)
        #f
        (declared-helper? var (car-slayer slayer)))))

; error checks
(define declared?
  (lambda (var slayer)
    (cond
      ((null? slayer) #f)
      ((declared-helper? var (car-slayer slayer)) #t)
      (else
       (declared? var (cdr slayer))))))

; declared in a state 
(define declared-helper?
  (lambda (var s)
    (cond
      ((null? s) #f)
      ((null? (var-list s)) #f)
      ((eq? (carvar s) var) #t)
      (else (declared-helper? var (cdrstate s))))))

; takes a variable and a state, returns #t if the value of variable is not 'error else #f
(define assigned-helper?
  (lambda (variable s)
    (cond
      ((not (declared-helper? variable s)) #f)
      ((eq? (carvar s) variable)
       (not (eq? (carval s) 'error)))
      (else
       (assigned-helper? variable (cdrstate s))))))

(define assigned?
  (lambda (variable slayer)
    (cond
      ((null? slayer) #f)
      ((assigned-helper? variable (car-slayer slayer)) #t)
      (else
       (assigned? variable (cdr-slayer slayer))))))

(define block?
  (lambda (stmt)
    (eq? (car stmt) 'begin)))

(define while-block?
  (lambda (stmt)
    (and (while? stmt) (block? (while-body-statement stmt)))))

(define continue?
  (lambda (stmt)
    (eq? (car stmt) 'continue)))

(define try-catch?
  (lambda (stmt)
    (and (eq? (car stmt) 'try) (null? (car (cdddr stmt))))))

(define try-catch-finally?
  (lambda (stmt)
    (and (eq? (car stmt) 'try) (not (null? (car (cdddr stmt)))))))

(define throw?
  (lambda (stmt)
    (eq? (car stmt) 'throw)))

; our wrappers for bool operations
; our and operator, takes two atoms of 'true or 'false
(define myand
  (lambda (bool1 bool2)
    (and bool1 bool2))) ; convert atom 'true or 'false to #t or #f else run into weird bugs 

; our or operator, takes two atoms of 'true or 'false
(define myor
  (lambda (bool1 bool2)
    (or bool1 bool2)))

; our not operator, takes an atom of 'true or 'false
(define mynot
  (lambda (bool)
    (not bool)))

(define booloperator
  (lambda (op)
    (lambda (bool1 bool2)
      (op bool1 bool2))))

(define noteq
 (lambda (bool1 bool2)
   (not (eq? bool1 bool2))))

; add variable to the first state list in state 
(define add-var-helper
  (lambda (variable value s)
    (cons (cons variable (var-list s)) (cons (cons value (value-list s)) '()))))

(define add-var
  (lambda (variable value slayer)
    (cons (add-var-helper variable value (car-slayer slayer)) (cdr-slayer slayer))))

; remove variable from the first state list in state 
(define remove-var-helper
  (lambda (variable s)
    (cond
      ((null? (var-list s)) s)
      ((eq? (car (var-list s)) variable) (cdrstate s))
      (else
       (add-var-helper (car (var-list s)) (car (value-list s)) (remove-var-helper variable (cdrstate s)))))))

(define remove-var
  (lambda (variable slayer)
    (cond
      ((null? slayer) slayer)
      (else 
       (cons (remove-var-helper variable (car-slayer slayer)) (cdr-slayer slayer))))))

; add layer to state each time a new block is entered
(define add-layer
  (lambda (slayer)
    (cons state slayer)))

(define remove-layer
  (lambda (slayer)
    (cdr slayer)))

  