#!/usr/bin/racket
#lang racket

(require "simpleParser.scm")
(require "abstractions.scm")
(require "helpers.scm")

(provide (all-defined-out))

; takes filename, parses file, evaluates parse tree, returns proper value
(define interpret
  (lambda (filename)
    (m-value-exp-value (m-value-exp 'return
                                    (m-state-stmt-list (parser filename) (m-state-declare 'return '(()())))))))

; takes a statement list and state, returns the state after executing all statements
(define m-state-stmt-list
  (lambda (stmt-list s)
    (if (null? stmt-list) s
        (m-state-stmt-list (cdr stmt-list) (m-state (car stmt-list) s)))))

; takes a statement and state, returns a new state after executing stmt
(define m-state
  (lambda (stmt s)
    (cond
      ((null? stmt) s)
      ((declarationandassignment? stmt)
       (m-state-assign (assignment-variable stmt) (assignment-expression stmt) (m-state-declare (declaration-variable stmt) s)))
      ((declaration? stmt) (m-state-declare (declaration-variable stmt) s))
      ((assignment? stmt) (m-state-assign (assignment-variable stmt) (assignment-expression stmt) s))
      ((ifthenelse? stmt) (m-state-if (if-condition stmt) (then-statement stmt) (else-statement stmt) s))
      ((ifthen? stmt) (m-state-if (if-condition stmt) (then-statement stmt) '() s))
      ((while? stmt) (m-state-while (while-condition stmt) (while-body-statement stmt) s))
      ((return? stmt) (m-state-return (return-expression stmt) s)))))

; takes a stmt and a state, assigns 'return variable to the variable/value in the stmt
(define m-state-return
  (lambda (expr s)
    (m-state-assign 'return expr s)))

; get the value of the tuple
(define m-value-exp-value car)

; get the state of the tuple
(define m-value-exp-state cadr)

; takes a value and a state, creates a tuple of the value and state
(define 2tuple
  (lambda (value s)
    (cons value (cons s '()))))

; takes an expression and state
; returns a tuple of the value of the expression and a possibly new state 
(define m-value-exp
  (lambda (exp s)
    (cond
      ;((number? exp) (2tuple (m-value-int exp s) s))
      ((int-exp? exp)
        (2tuple (m-value-exp-value (m-value-int exp s)) (m-value-exp-state (m-value-int exp s))))
      ((bool-exp? exp)
       (2tuple (m-value-exp-value (m-value-bool exp s)) (m-value-exp-state (m-value-bool exp s))))
      ;handles side effects
      ((assignment? exp)
       ; get the value of the assignment variable from the state after the assignment occurred
       ; return tuple of the value with the state after assignment
       (2tuple (m-value-exp-value (m-value-exp (assignment-variable exp)) (m-state-assign (assignment-variable exp) s))
               (m-state-assign (assignment-variable exp) s)))
      (else
       (2tuple (m-value-variable exp s)
               s)))))

; takes a variable and state, returns the value of the variable in the state
(define m-value-variable
  (lambda (variable s)
    (cond
      ((not (declared? variable s)) (error 'UndeclaredVariable))
      ((not (assigned? variable s)) (error 'UnassignedVariable))
      ((eq? (car (var-list s)) variable) (car (value-list s)))
      (else
       (m-value-variable variable (cdrstate s))))))

; takes a variable and a state, adds variable to state
(define m-state-declare
  (lambda (variable s)
    (cond
      ((declared? variable s) (error 'RedefiningVariable))
      (else
       (cons (cons variable (car s)) (cons (cons 'error (cadr s)) '()))))))

; assigns variable to the value of exp, updates the variable in the state, and returns the variable value
(define m-state-assign
  (lambda (variable exp s)
    (cond
      ((not (declared? variable s)) (error 'UndeclaredVariable))
      ((assignment? exp) ; side effects
       (m-state-assign variable (m-value-exp-value (m-value-exp exp s)) (m-value-exp-state (m-value-exp exp s))))
      (else
       (add-var variable (m-value-exp-value (m-value-exp exp s)) (remove-var variable (m-value-exp-state (m-value-exp exp s))))))))

; takes condition, then, else statements and state, returns a new state
(define m-state-if
  (lambda (if-cond then-stmt else-stmt s)
    (cond
      ((eq? (m-value-exp-value (m-value-exp if-cond s)) 'true)
       (m-state then-stmt (m-value-exp-state (m-value-exp if-cond s)))) ; accounts for if condition changes state
      (else
       (m-state else-stmt (m-value-exp-state (m-value-exp if-cond s)))))))

(define m-state-while
  (lambda (while-cond loop-body s)
    (cond
      ((eq? (m-value-exp-value (m-value-exp while-cond s)) 'true)
       (m-state while-cond (m-state loop-body (m-value-exp-state (m-value-exp while-cond s)))))
      (else
       (m-value-exp-state (m-value-exp while-cond s))))))

; takes an expression and state, returns the value of the expression and a possibly new state 
(define m-value-int
  (lambda (exp s)
    (cond
      ((null? exp) (2tuple 0 s))
      ((number? exp) (2tuple exp s)) ; '6
      ((eq? (operator exp) '+) (m-value-binaryop + exp s))
      ((subtraction? exp) (m-value-binaryop - exp s))
      ((negative? exp) (m-value-unaryop - exp s))
      ((eq? (operator exp) '*) (m-value-binaryop * exp s))
      ((eq? (operator exp) '/) (m-value-binaryop quotient exp s))
      ((eq? (operator exp) '%) (m-value-binaryop remainder exp s))
      (else (error 'badoperation "Unknown operator")))))

; takes an expression and state, returns 2tuple of atom 'true or 'false and state
(define m-value-bool
  (lambda (exp s)
    (cond
      ((null? exp) (error 'EmptyCondition))
      ((eq? exp 'true) (2tuple 'true s))
      ((eq? exp 'false) (2tuple 'false s))
      ((eq? (operator exp) '&&) (booltuple-to-atomtuple (m-value-booleanop myand exp s)))
      ((eq? (operator exp) '||) (booltuple-to-atomtuple (m-value-booleanop myor exp s)))
      ((eq? (operator exp) '==) (booltuple-to-atomtuple (m-value-binaryop eq? exp s)))
      ((eq? (operator exp) '!=) (booltuple-to-atomtuple (m-value-binaryop noteq exp s)))
      ((eq? (operator exp) '<) (booltuple-to-atomtuple (m-value-binaryop < exp s)))
      ((eq? (operator exp) '<=) (booltuple-to-atomtuple (m-value-binaryop <= exp s)))
      ((eq? (operator exp) '>) (booltuple-to-atomtuple (m-value-binaryop > exp s)))
      ((eq? (operator exp) '>=) (booltuple-to-atomtuple (m-value-binaryop >= exp s)))
      ((eq? (operator exp) '!) (booltuple-to-atomtuple (m-value-unaryop not exp s))) 
      (else (error 'badoperation "Unknown operator")))))

; takes a binary operator, expression, and state, returns 2-tuple of value of expression and the possibly new state
(define m-value-binaryop
  (lambda (op exp s)
    ; value is the operator applied to the values of the first and second operand
    ; state can be changed by the first operand then the second operand 
    (2tuple (op (m-value-exp-value (m-value-exp (operand1 exp) s)) (m-value-exp-value (m-value-exp (operand2 exp) s)))
               (m-value-exp-state (m-value-exp (operand2 exp) (m-value-exp-state (m-value-exp (operand1 exp) s)))))))

; takes an unary operator, expression, and state, returns 2-tuple of value of expression and the possibly new state
(define m-value-unaryop
  (lambda (op exp s)
    (2tuple (op (m-value-exp-value (m-value-exp (operand1 exp) s)))
               (m-value-exp-state (m-value-exp (operand1 exp) s)))))

(define booltuple-to-atomtuple
  (lambda (booltuple)
    (if (m-value-exp-value booltuple)
      (2tuple 'true (m-value-exp-state booltuple))
      (2tuple 'false (m-value-exp-state booltuple)))))

(define m-value-booleanop
  (lambda (op exp s)
    (2tuple (op (m-value-exp-value (m-value-exp (operand1 exp) s)) (m-value-exp-value (m-value-exp (operand2 exp) s)))
               (m-value-exp-state (m-value-exp (operand2 exp) (m-value-exp-state (m-value-exp (operand1 exp) s)))))))

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

; initializes a variable and returns the value of the initialized variable

(eq? (interpret "test/programs/1") 150)
(eq? (interpret "test/programs/2") -4)
(eq? (interpret "test/programs/3") 10)
(eq? (interpret "test/programs/4") 16)
(eq? (interpret "test/programs/5") 220)
(eq? (interpret "test/programs/6") 5)
(eq? (interpret "test/programs/7") 6)
(eq? (interpret "test/programs/8") 10)
(eq? (interpret "test/programs/9") 5)
(eq? (interpret "test/programs/10") -39)
;(interpret "test/programs/11")
;(interpret "test/programs/12")
;(interpret "test/programs/13")
;(interpret "test/programs/14")
(eq? (interpret "test/programs/15") 'true)
(eq? (interpret "test/programs/16") 100)
(eq? (interpret "test/programs/17") 'false)
(eq? (interpret "test/programs/18") 'true)
(eq? (interpret "test/programs/19") 2)
