#!/usr/bin/racket
#lang racket

(require "functionParser.scm")
(require "abstractions_part2.scm")
(require "helpers_part2.scm")

(provide (all-defined-out))

; takes filename, parses file, evaluates parse tree, returns proper value
(define interpret 
  (lambda (filename)
    (call/cc
     (lambda (return)
       (m-value-function 'main '() (m-state-stmt-list (parser filename) init-state errorbreak errorbreak errorcontinue errorthrow)
                         return errorbreak errorcontinue errorthrow)))))
          ;(m-state-stmt-list (parser filename) init-state return errorbreak errorcontinue errorthrow)))))
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
    (if (null? stmt-list) s
        (m-state-stmt-list (cdr stmt-list) (m-state (car stmt-list) s return break continue  throw) return break continue  throw)))) ; what if we get break outside of {}

; takes a statement and state layer, returns a new state after executing stmt
(define m-state
  (lambda (stmt s return break continue  throw)
    (cond
      ((null? stmt) s)
      ((not (list? stmt)) s)
      ((main-function? stmt) (add-function-binding stmt s main-func-env))
      ((function? stmt)  (add-function-binding stmt s get-func-env))
      ;((function-call? stmt) s)
      ((function-call? stmt) (merge-state (m-state-function (func-name stmt) (actual-params stmt) s return break continue throw) s)) ; update s with state returned by m-state-function
      ((try-catch? stmt) (try-catch-finally-cc (try-block stmt) (catch-block stmt) '() s return break continue throw))
      ((try-finally? stmt) (try-catch-finally-cc (try-block stmt) '(catch (e) ()) (finally-block stmt) s return break continue throw))
      ((try-catch-finally? stmt) (try-catch-finally-cc (try-block stmt) (catch-block stmt) (finally-block stmt) s return break continue throw))
      ((block? stmt) (m-state-block (cdr stmt) s return break continue throw))
      ((return? stmt) (return (m-state-assign 'return
                                              (m-value-exp (return-stmt stmt) s return break continue throw)
                                              (m-state-declare 'return
                                                               s return break continue throw)
                                              return break continue throw)))
      ((break? stmt) (break s))
      ((continue? stmt) (continue s))
      ((throw? stmt) (throw (m-state-assign 'throw (throw-exp stmt) s return break continue throw))) ; Assign throw the value of the value thrown
      ((declarationandassignment? stmt)
       (m-state-assign (assignment-variable stmt) (assignment-expression stmt) (m-state-declare (declaration-variable stmt) s return break continue throw) return break continue  throw))
      ((declaration? stmt) (m-state-declare (declaration-variable stmt) s return break continue throw))
      ((assignment? stmt) (m-state-assign (assignment-variable stmt) (assignment-expression stmt) s return break continue throw))
      ((ifthenelse? stmt) (m-state-if (if-condition stmt) (then-statement stmt) (else-statement stmt) s return break continue throw))
      ((ifthen? stmt) (m-state-if (if-condition stmt) (then-statement stmt) '() s return break continue  throw))
      ((while? stmt) (m-state-while (while-condition stmt) (while-body-statement stmt) s return  throw))
      ((unary? stmt) (m-state (operand1 stmt) s return break continue  throw))
      ((bool-exp? stmt) (m-state (operand2 stmt) (m-state (operand1 stmt) s return break continue  throw) return break continue throw))
      ((int-exp? stmt) (m-state (operand2 stmt) (m-state (operand1 stmt) s return break continue  throw) return break continue throw))
      (else s))))

;;;;;;;::::
; PART THREE
;;;;;;;;;;;;

(define merge-state
  (lambda (prev-s func-s)
    (append (remove-global-state func-s) (get-global-state prev-s))))

(define get-global-state
  (lambda (s)
    (cond
      ((null? (cdr s)) s)
      (else
       (get-global-state (cdr s))))))

(define remove-global-state
  (lambda (s)
    (cond
      ((null? (cdr s)) '())
      (else
       (cons (car s) (remove-global-state (cdr s)))))))

; function that returns the binding in scope
(define get-func-env
  (lambda (s)
    (cond
      ((null? (cdr s)) s) ;Check if global (bottom) layer
      (else
       (cdr s)))))

(define main-func-env
  (lambda (s)
    s))

; bind the function name to its closure
; closure is list that contains param, body, and function that returns binding in scope
(define add-function-binding
  (lambda (function s func-env) ;
    (cond
      ((eq? (func-name function) 'main)
       (add-var (func-name function) (list (param-body function) func-env) s))
      (else
       (add-var (func-name function) (list (param-body function) func-env) s)))))

; add nested functions to the state
(define m-state-function
  (lambda (func-name actual-params s return break continue throw)
    (call/cc
     (lambda (return)
       (m-state-stmt-list (func-body (m-value-variable func-name s errorbreak errorbreak errorcontinue errorthrow))
                          (add-parameters (formal-params (m-value-variable func-name s errorbreak errorbreak errorcontinue errorthrow)) ; formal parameters
                                          (var-list-to-val-list actual-params s errorbreak errorbreak errorcontinue errorthrow)
                                          (add-layer ((binding (m-value-variable func-name s errorbreak errorbreak errorcontinue errorthrow))
                                                      s)) ; get the function that will return the function closure and run the function on the current state to get function environment
                                          return break continue throw)
                          return errorbreak errorcontinue errorthrow)))))
    

; get the value of a function
(define m-value-function
  (lambda (func-name actual-params s return break continue throw)
    (m-value-variable 'return
                      (call/cc
                       (lambda (return)
                         (m-state-stmt-list (func-body (m-value-variable func-name s errorbreak errorbreak errorcontinue errorthrow))
                                            (add-parameters (formal-params (m-value-variable func-name s errorbreak errorbreak errorcontinue errorthrow)) ; formal parameters
                                                            (var-list-to-val-list actual-params s errorbreak errorbreak errorcontinue errorthrow)
                                                            (add-layer ((binding (m-value-variable func-name s errorbreak errorbreak errorcontinue errorthrow))
                                                                        s)) ; get the function that will return the function closure and run the function on the current state to get function environment
                                                            return break continue throw)
                                            return errorbreak errorcontinue errorthrow)))
                      return break continue throw)))

(define var-list-to-val-list
  (lambda (actual-params s return break continue throw)
    (cond
      ((null? actual-params) '())
      ((or (bool? (car actual-params))
            (number? (car actual-params)))
       (cons (car actual-params) (var-list-to-val-list (cdr actual-params) s return break continue throw)))
      (else
       (cons (m-value-exp (car actual-params) s return break continue throw)
             (var-list-to-val-list (cdr actual-params) s return break continue throw))))))
       
(define bool? ; TODO: move to abstractions/helpers
  (lambda (exp)
    (or (eq? 'true exp) (eq? 'false exp))))
       

; return the state where all the formal params is binded to the actual params 
(define add-parameters
  (lambda (formal-params actual-params s return break continue throw)
    (cond
      ((null? formal-params) s)
      (else
       (add-parameters (cdr formal-params) (cdr actual-params) (add-parameter (car formal-params) (car actual-params) s return break continue throw)
                       return break continue throw)))))

; returns the state where the formal-param is actual-param is declared 
(define add-parameter
  (lambda (formal-param actual-param s return break continue throw)
    (m-state-assign formal-param actual-param (m-state-declare formal-param s return break continue throw) return break continue throw)))
    ;(m-state-assign (assignment-variable stmt) (assignment-expression stmt) (m-state-declare (declaration-variable stmt) s return break continue throw) return break continue  throw))
    ;(m-state-assign (m-state-declare formal-param s) (m-state-value actual-param s) )))
    

(define m-state-block
  (lambda (stmt-list s return break continue throw)
    (remove-layer
     (call/cc
      (lambda (safebreak)
        (break
         (remove-layer
          (call/cc
           (lambda (break2)
             (continue
              (remove-layer
               (call/cc
                (lambda (continue2)
                  (throw
                   (remove-layer
                    (call/cc
                     (lambda (throw2)
                       (safebreak (m-state-stmt-list stmt-list (add-layer s) return break2 continue2 throw2)))))))))))))))))))


; takes a stmt and a state, assigns 'return variable to the variable/value in the stmt
(define m-state-return
  (lambda (expr s)
    (m-state-assign 'return expr s)))

; takes a variable and a state, adds variable to state
(define m-state-declare
  (lambda (variable s return break continue throw)
    (cond
      ((declared-in-current-layer? variable s) (error 'RedefiningVariable))
      (else
       (add-var variable 'error s)))))
       ;(cons (cons variable (car s)) (cons (cons 'error (cadr s)) '()))))))

; assigns variable to the value of exp, updates the variable in the state, and returns the variable value
(define m-state-assign
  (lambda (variable exp s return break continue  throw)
    (cond
      ((not (declared? variable (m-state exp s return break continue throw))) (error 'UndeclaredVariable))
      (else
       (removeadd variable (m-value-exp exp s return break continue  throw) s)))))
       ;(add-var variable (m-value-exp exp s) (remove-var variable (m-state exp s)))))))

; check to see if variable is in the state
; if it is in the state, break with (removeadd variable s s-layer)

(define inlayer?
  (lambda (variable s)
    (cond
      ((null? s) #f)
      ((null? (var-list s)) #f)
      ((eq? (car (var-list s)) variable) #t)
      (else
       (inlayer? variable (cdrstate s))))))

       
(define removeadd 
  (lambda (variable value s-layer)
    (cond
      ((null? s-layer) s-layer)
      ((inlayer? variable (car-slayer s-layer))
       (cons (removeaddlayer variable value (car-slayer s-layer)) (cdr-slayer s-layer))) ; remove and add variable in this layer, returns a layer
      (else
       (cons (car-slayer s-layer) (removeadd variable value (cdr-slayer s-layer)))))))

;(define removeadd
;  (lambda (variable value s-layer)
;    (call/cc
;     (lambda (break)
;       (cond
;         ((null? (cdr-slayer s-layer))
;          (cons (removeaddhelper variable value (car-slayer s-layer) (cdr-slayer s-layer) break) '()))
 ;        (else
;          (cons (removeaddhelper variable value (car-slayer s-layer) (cdr-slayer s-layer) break)
;                (removeadd variable value (cdr-slayer s-layer)))))))))


(define removeaddlayer
  (lambda (variable value layer)
    (cond
      ((null? layer) layer)
      ((eq? (car (var-list layer)) variable)
       (list (var-list layer) (cons value (cdr (value-list layer)))))
      (else
       (add-var-helper (car (var-list layer)) (car (value-list layer)) (removeaddlayer variable value (cdrstate layer)))))))

;(define removeaddhelper-break
;  (lambda (variable value s breakstate)
;       (cond
;         ((null? (var-list s)) s)
;         ((eq? (car (var-list s)) variable)
;          (break (list (var-list s) (cons value (cdr (value-list s))))))
;         (else
;          (add-var-helper (car (var-list s)) (car (value-list s)) (removeaddhelper-break variable value (cdrstate s) breakstate))))))


; takes condition, then, else statements and state, returns a new state
(define m-state-if
  (lambda (if-cond then-stmt else-stmt s return break continue  throw)
    (cond
      ((m-value-exp if-cond (m-state if-cond s return break continue  throw) return break continue  throw)
       (m-state then-stmt (m-state (operand2 if-cond) (m-state (operand1 if-cond) s return break continue  throw) return break continue  throw) return break continue  throw))
      (else
       (m-state else-stmt (m-state (operand2 if-cond) (m-state (operand1 if-cond) s return break continue  throw) return break continue  throw) return break continue  throw)))))

(define m-state-while
  (lambda (while-cond loop-body s return throw)
    (call/cc
     (lambda (break)
        (m-state-while-loop while-cond loop-body s return break break throw))))) 

; loop body can contain block stmt
(define m-state-while-loop
  (lambda (while-cond loop-body s return break continue throw)
    (cond
         ((eq? 'true (m-value-exp while-cond s return break continue throw))
          (m-state-while-loop while-cond loop-body
                              (m-state-with-continue loop-body (m-state while-cond s return break continue throw) return break throw) return break continue throw))
         (else
          (m-state while-cond s return break continue  throw)))))

(define m-state-with-continue
  (lambda (loop-body s return break throw)
    (call/cc
     (lambda (continue)
       (cond
         ((block? loop-body) (m-state-block loop-body s return break continue throw))
         (else
          (m-state loop-body s return break continue  throw)))))))

; takes an expression and state
; returns a tuple of the value of the expression and a possibly new state 
(define m-value-exp
  (lambda (exp s return break continue  throw)
    (cond
      ;((function-call? exp) (m-value-function (func-name exp) (actual-params exp) s return break continue throw))
      ((int-exp? exp) (m-value-int exp s return break continue  throw))
      ((bool-exp? exp) (m-value-bool exp s return break continue  throw))
      ((not (list? exp)) (m-value-variable exp s return break continue  throw))
      ((function-call? exp) (m-value-function (func-name exp) (actual-params exp) s return break continue throw))
      (else
       (m-value-variable (operand1 exp) (m-state exp s return break continue throw) return break continue throw)))))

; takes a variable and state, returns the value of the variable in the state
(define m-value-variable
  (lambda (variable s return break continue  throw)
    (cond
      ((not (declared? variable s)) (error 'UndeclaredVariable))
      ((not (assigned? variable s)) (error 'UnassignedVariable))
      ((null? (car (car-slayer s))) (m-value-variable variable (cdr-slayer s) return break continue  throw))
      ((eq? (carvar (car-slayer s)) variable) (carval (car-slayer s)))
      (else
       (m-value-variable variable (cons (cdrstate (car-slayer s)) (cdr-slayer s)) return break continue  throw)))))


; takes an expression and state, returns the value of the expression and a possibly new state 
(define m-value-int
  (lambda (exp s return break continue  throw)
    (cond
      ((null? exp) 0)
      ((number? exp) exp) ; '6
      ((eq? (operator exp) '+) (m-value-binaryop + exp s return break continue  throw))
      ((subtraction? exp) (m-value-binaryop - exp s return break continue  throw))
      ((negative? exp) (m-value-unaryop - exp s return break continue  throw))
      ((eq? (operator exp) '*) (m-value-binaryop * exp s return break continue  throw))
      ((eq? (operator exp) '/) (m-value-binaryop quotient exp s return break continue  throw))
      ((eq? (operator exp) '%) (m-value-binaryop remainder exp s return break continue  throw))
      (else (error 'badoperation "Unknown operator")))))

; takes an expression and state, returns 2tuple of atom 'true or 'false and state
(define m-value-bool
  (lambda (exp s return break continue  throw)
    (cond
      ((null? exp) (error 'EmptyCondition))
      ((eq? exp 'true) 'true)
      ((eq? exp 'false) 'false)
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
  (lambda (op exp s return break continue  throw)
    (op (m-value-exp (operand1 exp) s return break continue  throw) (m-value-exp (operand2 exp) (m-state (operand1 exp) s return break continue  throw) return break continue  throw))))

; takes an unary operator, expression, and state, returns 2-tuple of value of expression and the possibly new state
(define m-value-unaryop
  (lambda (op exp s return break continue  throw)
    (cond
      ((number? (op (m-value-exp (operand1 exp) s return break continue  throw)))
       (op (m-value-exp (operand1 exp) s return break continue  throw)))
      (else
       (op (m-value-exp (operand1 exp) s return break continue  throw))))))

; takes boolean operator, expression, and state and returns tuple of #t or #f and state
(define m-value-booleanop
  (lambda (op exp s return break continue  throw)
    (op (m-value-exp (operand1 exp) s return break continue  throw) (m-value-exp (operand2 exp) (m-state (operand1 exp) s return break continue  throw) return break continue  throw))))

(define try-catch-finally-cc
  (lambda (try-block catch-block finally-block s return break continue throw)
    (m-state-block finally-block
                   (call/cc
                    (lambda (nothrow)
                      (remove-var (catch-exception catch-block)
                                  (m-state-block (catch-stmt catch-block) ; TODO: abstract
                                                 (remove-var 'throw
                                                             ((lambda (state)
                                                                (m-state-assign (catch-exception catch-block)
                                                                                (m-value-variable 'throw
                                                                                                  state
                                                                                                  return break continue throw)
                                                                                state
                                                                                return break continue throw))
                                                              (m-state-declare (catch-exception catch-block) ;This makes the variable e in the outermost layer. Should be a layer in
                                                                               (call/cc
                                                                                (lambda (throw2)
                                                                                  (nothrow (m-state-block try-block
                                                                                                          (m-state-declare 'throw s return break continue throw2)
                                                                                                          return break continue throw2))))
                                                                               return break continue throw)))
                                                             
                                                             return break continue throw))))
    return break continue throw)))

(interpret "test/part3/14")
(interpret "test/part3/1")
(interpret "test/part3/2")
(interpret "test/part3/3")
(interpret "test/part3/4")
(interpret "test/part3/5")
(interpret "test/part3/6")
(interpret "test/part3/7")
(interpret "test/part3/8")
(interpret "test/part3/9")
(interpret "test/part3/10")
(interpret "test/part3/11")
;(interpret "test/part3/12")
(interpret "test/part3/15")