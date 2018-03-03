#!/usr/bin/racket
#lang racket

(require "simpleParser.scm")
(require "abstractions_part2.scm")
(require "helpers_part2.scm")

(provide (all-defined-out))

; takes filename, parses file, evaluates parse tree, returns proper value
(define interpret 
  (lambda (filename)
    (call/cc
     (lambda (return)
      (m-state-stmt-list (parser filename) init-state return)))))
    ;(m-value-exp 'return (m-state-stmt-list (parser filename) (m-state-declare 'return init-state)))))

; takes a statement list and state, returns the state after executing all statements
(define m-state-stmt-list
  (lambda (stmt-list s return)
    (if (null? stmt-list) s
        (m-state-stmt-list (cdr stmt-list) (m-state (car stmt-list) s return) return))))

; takes a statement and state layer, returns a new state after executing stmt
(define m-state
  (lambda (stmt s return)
    (cond
      ((null? stmt) s)
      ((not (list? stmt)) s)
      ((block? stmt) (remove-layer (m-state-stmt-list (cdr stmt) (add-layer s) return)))
      ((return? stmt) (return (m-value-exp (return-stmt stmt) s return)))
      ((declarationandassignment? stmt)
       (m-state-assign (assignment-variable stmt) (assignment-expression stmt) (m-state-declare (declaration-variable stmt) s return) return))
      ((declaration? stmt) (m-state-declare (declaration-variable stmt) s return))
      ((assignment? stmt) (m-state-assign (assignment-variable stmt) (assignment-expression stmt) s return))
      ((ifthenelse? stmt) (m-state-if (if-condition stmt) (then-statement stmt) (else-statement stmt) s return))
      ((ifthen? stmt) (m-state-if (if-condition stmt) (then-statement stmt) '() s return))
      ((while? stmt) (m-state-while (while-condition stmt) (while-body-statement stmt) s return))
      ;((return? stmt) (m-state-return (return-expression stmt) s))
      ((unary? stmt) (m-state (operand1 stmt) s return))
      ((bool-exp? stmt) (m-state (operand2 stmt) (m-state (operand1 stmt) s return) return))
      ((int-exp? stmt) (m-state (operand2 stmt) (m-state (operand1 stmt) s return) return))
      (else
       s))))

; takes a stmt and a state, assigns 'return variable to the variable/value in the stmt
(define m-state-return
  (lambda (expr s)
    (m-state-assign 'return expr s)))

; takes a variable and a state, adds variable to state
(define m-state-declare
  (lambda (variable s return)
    (cond
      ((declared? variable s) (error 'RedefiningVariable))
      (else
       (add-var variable 'error s)))))
       ;(cons (cons variable (car s)) (cons (cons 'error (cadr s)) '()))))))

; assigns variable to the value of exp, updates the variable in the state, and returns the variable value
(define m-state-assign
  (lambda (variable exp s return)
    (cond
      ((not (declared? variable (m-state exp s return))) (error 'UndeclaredVariable))
      (else
       (removeadd variable (m-value-exp exp s return) s)))))
       ;(add-var variable (m-value-exp exp s) (remove-var variable (m-state exp s)))))))

(define removeadd
  (lambda (variable value s-layer)
    (cond
      ((null? (cdr-slayer s-layer))
       (cons (removeaddhelper variable value (car-slayer s-layer)) '()))
      (else
       (cons (removeaddhelper variable value (car-slayer s-layer))
             (removeadd variable value (cdr-slayer s-layer)))))))

(define removeaddhelper
  (lambda (variable value s)
    (call/cc
     (lambda (break)
       (cond
         ((null? (var-list s)) s)
         ((eq? (car (var-list s)) variable)
          (break (list (var-list s) (cons value (cdr (value-list s))))))
         (else
          (add-var-helper (car (var-list s)) (car (value-list s)) (removeaddhelper variable value (cdrstate s)))))))))

; s is layered state
(define removeandadd*
  (lambda (variable value s return)
    (cond
      ((null? s) (return s))
      (else 
       (removeandadd* variable value (cdr-slayer s)
                      (lambda (var1 val1) (removeandadd variable value (var-list (car-slayer s)) (value-list (car-slayer s))
                                                        (lambda (var2 val2) (return (cons (list var1 val1) (list var2 val2)))))))))))

(define removeandadd
  (lambda (variable value var-list value-list return)
    (cond
      ((null? var-list) (return var-list value-list))
      ((eq? (car var-list) variable)
       (return var-list (cons value (cdr value-list))))
      (else
       (removeandadd variable value (cdr var-list) (cdr value-list) (lambda (var2 value2) (return (cons (car var-list) var2) (cons (car value-list) value2))))))))

; takes condition, then, else statements and state, returns a new state
(define m-state-if
  (lambda (if-cond then-stmt else-stmt s return)
    (cond
      ((m-value-exp if-cond (m-state if-cond s return) return)
       (m-state then-stmt (m-state (operand2 if-cond) (m-state (operand1 if-cond) s return) return) return))
      (else
       (m-state else-stmt (m-state (operand2 if-cond) (m-state (operand1 if-cond) s return) return) return)))))

; takes condition, loop body, and state, returns state
(define m-state-while
  (lambda (while-cond loop-body s return)
    (cond
      ((m-value-exp while-cond s return)
       (m-state-while while-cond loop-body (m-state loop-body (m-state while-cond s return) return) return))
      (else
        (m-state while-cond s return)))))

; takes an expression and state
; returns a tuple of the value of the expression and a possibly new state 
(define m-value-exp
  (lambda (exp s return)
    (cond
      ((int-exp? exp) (m-value-int exp s return))
      ((bool-exp? exp) (m-value-bool exp s return))
      ((not (list? exp)) (m-value-variable exp s return))
      (else
       (m-value-variable (operand1 exp) (m-state exp s return) return)))))

; takes a variable and state, returns the value of the variable in the state
(define m-value-variable
  (lambda (variable s return)
    (cond
      ((not (declared? variable s)) (error 'UndeclaredVariable))
      ((not (assigned? variable s)) (error 'UnassignedVariable))
      ((null? (car (car-slayer s))) (m-value-variable variable (cdr-slayer s) return))
      ((eq? (carvar (car-slayer s)) variable) (carval (car-slayer s)))
      (else
       (m-value-variable variable (cons (cdrstate (car-slayer s)) (cdr-slayer s)) return)))))


; takes an expression and state, returns the value of the expression and a possibly new state 
(define m-value-int
  (lambda (exp s return )
    (cond
      ((null? exp) 0)
      ((number? exp) exp) ; '6
      ((eq? (operator exp) '+) (m-value-binaryop + exp s return))
      ((subtraction? exp) (m-value-binaryop - exp s return))
      ((negative? exp) (m-value-unaryop - exp s return))
      ((eq? (operator exp) '*) (m-value-binaryop * exp s return))
      ((eq? (operator exp) '/) (m-value-binaryop quotient exp s return))
      ((eq? (operator exp) '%) (m-value-binaryop remainder exp s return))
      (else (error 'badoperation "Unknown operator")))))

; takes an expression and state, returns 2tuple of atom 'true or 'false and state
(define m-value-bool
  (lambda (exp s return)
    (cond
      ((null? exp) (error 'EmptyCondition))
      ((eq? exp 'true) #t)
      ((eq? exp 'false) #f)
      ((eq? (operator exp) '&&) (m-value-booleanop myand exp s return))
      ((eq? (operator exp) '||) (m-value-booleanop myor exp s return))
      ((eq? (operator exp) '==) (m-value-booleanop eq? exp s return))
      ((eq? (operator exp) '!=) (m-value-booleanop noteq exp s return))
      ((eq? (operator exp) '<) (m-value-booleanop < exp s return))
      ((eq? (operator exp) '<=) (m-value-booleanop <= exp s return))
      ((eq? (operator exp) '>) (m-value-booleanop > exp s return))
      ((eq? (operator exp) '>=) (m-value-booleanop >= exp s return))
      ((eq? (operator exp) '!) (m-value-unaryop not exp s return))
      (else (error 'badoperation "Unknown operator")))))

; takes a binary operator, expression, and state, returns 2-tuple of value of expression and the possibly new state
(define m-value-binaryop
  (lambda (op exp s return)
    (op (m-value-exp (operand1 exp) s return) (m-value-exp (operand2 exp) (m-state (operand1 exp) s return) return))))

; takes an unary operator, expression, and state, returns 2-tuple of value of expression and the possibly new state
(define m-value-unaryop
  (lambda (op exp s return)
    (cond
      ((number? (op (m-value-exp (operand1 exp) s return)))
       (op (m-value-exp (operand1 exp) s return)))
      (else
       (op (m-value-exp (operand1 exp) s return))))))

; takes boolean operator, expression, and state and returns tuple of #t or #f and state
(define m-value-booleanop
  (lambda (op exp s return)
    (op (m-value-exp (operand1 exp) s return) (m-value-exp (operand2 exp) (m-state (operand1 exp) s return) return))))


