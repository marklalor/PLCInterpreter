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
          (m-state-stmt-list (parser filename) init-state return errorbreak errorcontinue errorthrow)))))
    ;(m-value-exp 'return (m-state-stmt-list (parser filename) (m-state-declare 'return init-state)))))

(define errorbreak
  (lambda (break)
    (error 'badbreak)))

(define errorcontinue
  (lambda (continue)
    (error 'badcontinue)))

(define errorthrow
  (lambda (throw)
    (error 'badthrow)))

; takes a statement list and state, returns the state after executing all statements
(define m-state-stmt-list
  (lambda (stmt-list s return break continue throw)
    (if (null? stmt-list) (throw s)
        (m-state-stmt-list (cdr stmt-list) (m-state (car stmt-list) s return break continue throw) return break continue throw)))) ; what if we get break outside of {}

(define m-state-stmt-list-cps
  (lambda (stmt-list s return break continue throw)
    (cond
      ((null? stmt-list) s)
      ((throw? (car stmt-list)) (break (m-state (car stmt-list) s return break continue throw)))
      (else
       (m-state-stmt-list-cps (cdr stmt-list) (m-state (car stmt-list) s return break continue throw) return break continue throw)))))

    ;(if (null? stmt-list) (throw s)
    ;  (m-state (car stmt-list) s return break continue
    ;           (lambda (s2)
    ;             (m-state-stmt-list-cps (cdr stmt-list) s2 return break continue
    ;                                    (lambda (s3) (throw s3))))))))

; takes a statement and state layer, returns a new state after executing stmt
(define m-state
  (lambda (stmt s return break continue throw)
    (cond
      ((null? stmt) s)
      ((not (list? stmt)) s)
      ((try-catch? stmt) (try-catch-finally-break (try-block stmt) (catch-block stmt) '() s return break continue (lambda (v) v)))
      ((try-catch-finally? stmt) (try-catch-finally (try-block stmt) (catch-block stmt) (finally-block stmt) s return break continue (lambda (v) v)))
      ((while-block? stmt) (remove-layer (m-state-while (while-condition stmt) (while-body-statement stmt) (add-layer s) return throw)))
      ((block? stmt) (remove-layer (m-state-stmt-list (cdr stmt) (add-layer s) return break continue throw)))
      ((return? stmt) (return (m-value-exp (return-stmt stmt) s return break continue throw)))
      ((break? stmt) (break s))
      ((continue? stmt) (continue s))
      ((throw? stmt) (throw (throw-exp stmt)))
      ((declarationandassignment? stmt)
       (m-state-assign (assignment-variable stmt) (assignment-expression stmt) (m-state-declare (declaration-variable stmt) s return break continue throw) return break continue throw))
      ((declaration? stmt) (m-state-declare (declaration-variable stmt) s return break continue throw))
      ((assignment? stmt) (m-state-assign (assignment-variable stmt) (assignment-expression stmt) s return break continue throw))
      ((ifthenelse? stmt) (m-state-if (if-condition stmt) (then-statement stmt) (else-statement stmt) s return break continue throw))
      ((ifthen? stmt) (m-state-if (if-condition stmt) (then-statement stmt) '() s return break continue throw))
      ((while? stmt) (m-state-while (while-condition stmt) (while-body-statement stmt) s return throw))
      ;((return? stmt) (m-state-return (return-expression stmt) s))
      ((unary? stmt) (m-state (operand1 stmt) s return break continue throw))
      ((bool-exp? stmt) (m-state (operand2 stmt) (m-state (operand1 stmt) s return break continue throw) return break continue throw))
      ((int-exp? stmt) (m-state (operand2 stmt) (m-state (operand1 stmt) s return break continue throw) return break continue throw))
      (else
       s))))

; takes a stmt and a state, assigns 'return variable to the variable/value in the stmt
(define m-state-return
  (lambda (expr s)
    (m-state-assign 'return expr s)))

; takes a variable and a state, adds variable to state
(define m-state-declare
  (lambda (variable s return break continue throw)
    (cond
      ((declared? variable s) (error 'RedefiningVariable))
      (else
       (add-var variable 'error s)))))
       ;(cons (cons variable (car s)) (cons (cons 'error (cadr s)) '()))))))

; assigns variable to the value of exp, updates the variable in the state, and returns the variable value
(define m-state-assign
  (lambda (variable exp s return break continue throw)
    (cond
      ((not (declared? variable (m-state exp s return break continue throw))) (error 'UndeclaredVariable))
      (else
       (removeadd variable (m-value-exp exp s return break continue throw) s)))))
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

; takes condition, then, else statements and state, returns a new state
(define m-state-if
  (lambda (if-cond then-stmt else-stmt s return break continue throw)
    (cond
      ((m-value-exp if-cond (m-state if-cond s return break continue throw) return break continue throw)
       (m-state then-stmt (m-state (operand2 if-cond) (m-state (operand1 if-cond) s return break continue throw) return break continue throw) return break continue throw))
      (else
       (m-state else-stmt (m-state (operand2 if-cond) (m-state (operand1 if-cond) s return break continue throw) return break continue throw) return break continue throw)))))

(define m-state-while
  (lambda (while-cond loop-body s return throw)
    (call/cc
     (lambda (break)
        (m-state-while-loop while-cond loop-body s return break break throw))))) ; need to remove top layer if we breaked inside {}

; loop body can contain block stmt
(define m-state-while-loop
  (lambda (while-cond loop-body s return break continue throw)
    (cond
         ((m-value-exp while-cond s return break continue throw)
          (m-state-while-loop while-cond loop-body
                              (m-state-while-continue while-cond loop-body s return break throw) return break continue throw))
                              ;(m-state loop-body) (m-state while-cond s return break continue)))
         (else
          (m-state while-cond s return break continue throw)))))

(define m-state-while-continue
  (lambda (while-cond loop-body s return break throw)
    (call/cc
     (lambda (continue)
       (cond
         ((block? loop-body) (m-state-stmt-list (cdr loop-body) (m-state while-cond s return break continue throw) return break continue throw))
         (else
          (m-state loop-body (m-state while-cond s return break continue) return break continue throw)))))))

; takes an expression and state
; returns a tuple of the value of the expression and a possibly new state 
(define m-value-exp
  (lambda (exp s return break continue throw)
    (cond
      ((int-exp? exp) (m-value-int exp s return break continue throw))
      ((bool-exp? exp) (m-value-bool exp s return break continue throw))
      ((not (list? exp)) (m-value-variable exp s return break continue throw))
      (else
       (m-value-variable (operand1 exp) (m-state exp s return break) return break continue throw)))))

; takes a variable and state, returns the value of the variable in the state
(define m-value-variable
  (lambda (variable s return break continue throw)
    (cond
      ((not (declared? variable s)) (error 'UndeclaredVariable))
      ((not (assigned? variable s)) (error 'UnassignedVariable))
      ((null? (car (car-slayer s))) (m-value-variable variable (cdr-slayer s) return break continue throw))
      ((eq? (carvar (car-slayer s)) variable) (carval (car-slayer s)))
      (else
       (m-value-variable variable (cons (cdrstate (car-slayer s)) (cdr-slayer s)) return break continue throw)))))


; takes an expression and state, returns the value of the expression and a possibly new state 
(define m-value-int
  (lambda (exp s return break continue throw)
    (cond
      ((null? exp) 0)
      ((number? exp) exp) ; '6
      ((eq? (operator exp) '+) (m-value-binaryop + exp s return break continue throw))
      ((subtraction? exp) (m-value-binaryop - exp s return break continue throw))
      ((negative? exp) (m-value-unaryop - exp s return break continue throw))
      ((eq? (operator exp) '*) (m-value-binaryop * exp s return break continue throw))
      ((eq? (operator exp) '/) (m-value-binaryop quotient exp s return break continue throw))
      ((eq? (operator exp) '%) (m-value-binaryop remainder exp s return break continue throw))
      (else (error 'badoperation "Unknown operator")))))

; takes an expression and state, returns 2tuple of atom 'true or 'false and state
(define m-value-bool
  (lambda (exp s return break continue throw)
    (cond
      ((null? exp) (error 'EmptyCondition))
      ((eq? exp 'true) #t)
      ((eq? exp 'false) #f)
      ((eq? (operator exp) '&&) (m-value-booleanop myand exp s return break continue throw))
      ((eq? (operator exp) '||) (m-value-booleanop myor exp s return break continue throw))
      ((eq? (operator exp) '==) (m-value-booleanop eq? exp s return break continue throw))
      ((eq? (operator exp) '!=) (m-value-booleanop noteq exp s return break continue throw))
      ((eq? (operator exp) '<) (m-value-booleanop < exp s return break continue throw))
      ((eq? (operator exp) '<=) (m-value-booleanop <= exp s return break continue throw))
      ((eq? (operator exp) '>) (m-value-booleanop > exp s return break continue throw))
      ((eq? (operator exp) '>=) (m-value-booleanop >= exp s return break continue throw))
      ((eq? (operator exp) '!) (m-value-unaryop not exp s return break continue throw))
      (else (error 'badoperation "Unknown operator")))))

; takes a binary operator, expression, and state, returns 2-tuple of value of expression and the possibly new state
(define m-value-binaryop
  (lambda (op exp s return break continue throw)
    (op (m-value-exp (operand1 exp) s return break continue throw) (m-value-exp (operand2 exp) (m-state (operand1 exp) s return break continue throw) return break continue throw))))

; takes an unary operator, expression, and state, returns 2-tuple of value of expression and the possibly new state
(define m-value-unaryop
  (lambda (op exp s return break continue throw)
    (cond
      ((number? (op (m-value-exp (operand1 exp) s return break continue throw)))
       (op (m-value-exp (operand1 exp) s return break continue throw)))
      (else
       (op (m-value-exp (operand1 exp) s return break continue throw))))))

; takes boolean operator, expression, and state and returns tuple of #t or #f and state
(define m-value-booleanop
  (lambda (op exp s return break continue throw)
    (op (m-value-exp (operand1 exp) s return break continue throw) (m-value-exp (operand2 exp) (m-state (operand1 exp) s return break continue throw) return break continue throw))))

(define try-catch-finally-break
  (lambda (try-block catch-block finally-block s return break continue throw)
    (call/cc
     (lambda (try-break)
       (try-catch-finally try-block catch-block finally-block s return try-break continue throw)))))
    
(define try-catch-finally
  (lambda (try-block catch-block finally-block s return break continue throw)
    (m-state-stmt-list-cps try-block s return break continue
                       (lambda (try-return)
                         (cond
                           ((and (list? try-return) (not (null? finally-block))) ; finally block exist 
                            (m-state-stmt-list-cps finally-block try-return return break continue
                                               (lambda (finally-return) (throw finally-return))))
                           ((and (list? try-return) (null? finally-block)) (throw try-return))
                           (else
                            (m-state-stmt-list-cps (catch-stmt catch-block) (m-state-assign (catch-exception catch-block) try-return (m-state-declare (catch-exception catch-block) s return break continue throw) return break continue throw) return break continue
                                               (lambda (catch-return)
                                                 (cond
                                                   (m-state-stmt-list-cps finally-block catch-return return break continue
                                                                    (lambda (finally-return) (throw finally-return))))))))))))

;(define try-catch-finally-cps
;  (lambda (try-block catch-block finally-block s return break continue throw)
;      (m-state (car try-block) s return break continue
;                       (lambda (try-return)
;                         ((and (list? try-return) (not (null? cdr try-block))) ; state returned 
;                          (try-catch-finally-cps (cdr try-block) catch-block finally-block try-return return break continue throw))
;                         ((and (list? try-return) (null? finally-block)) (throw try-return)
;                         ((not (null? finally-block))
;                          (throw (m-state-stmt-list finally-block try-return return break continue throw)))
;                         (else
;                          (m-state-stmt-list catch
                          



                       

(define try '(try
   ((= x 20) (if (> x 0) (throw 10)) (= x (+ x 5)))
   (catch (e) ((= x e)))
   (finally ((= x (+ x 100))))))

(define t '((= x 20) (if (> x 0) (throw 10)) (= x (+ x 5))))
(define c '(catch (e) ((= x e))))

(try-catch-finally-break t c '() '(((x)(10))) (lambda (v) v) errorbreak errorcontinue (lambda (v) v))
;(interpret "test/part2/10")
;(interpret "test/part2/9")
;(interpret "test/part2/1")
;(interpret "test/part2/2")
;(interpret "test/part2/3")
;(interpret "test/part2/4")
;(interpret "test/part2/6")
;(interpret "test/part2/7")
;(interpret "test/part2/11")
