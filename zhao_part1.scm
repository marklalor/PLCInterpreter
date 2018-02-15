(require "simpleParser.scm")
(require "abstractions.scm")

; takes filename, parses file, evaluates parse tree, returns proper value
(define interpret
  (lambda (filename)
    (m-value 'return (m-state-stmt-list (parser filename) '(()())))))

; takes a statement list and state, returns the state after executing all statements
(define m-state-stmt-list
  (lambda (stmt-list s)
    (if (null? stmt-list) s
        (m-state-stmt-list (cdr stmt-list) (m-state (car stmt-list) s)))))

; takes a statement and state, returns a new state after executing stmt
(define m-state
  (lambda (stmt s)
    (cond
      ((null? stmt) '(()()))
      ;((declareandassign? stmt)
       ;(m-state-assign (variable stmt) (expression stmt) (m-state-declare variable s)))
      ((declaration? stmt) (m-state-declare (variable stmt) s))
      ((assignment? stmt) (m-state-assign (variable stmt) (expression stmt) s))
      ((eq? 'return (car stmt)) (m-state-assign (car stmt) (cadr stmt) s)))))


(define m-value
  (lambda (exp s)
    (cond
      ((number? exp) (m-value-int exp))
      ((list? exp) (m-value-int exp))
      (else
       (m-value-variable exp s)))))

(define m-value-variable
  (lambda (variable s)
    (cond
      ((null? (var-list s)) error 'error "Unknown variable")
      ((eq? (car (var-list s)) variable) (car (value-list s)))
      (else
       (m-value-variable variable (removefirstvar s))))))

(define m-state-declare
  (lambda (variable s)
    (cons (cons variable (car s)) (cons (cons 'error (cadr s)) '()))))

(define declaration?
  (lambda (stmt)
    (and (null? (cddr stmt)) (eq? (car stmt) 'var))))
                           
; assigns variable to the value of exp, updates the variable in the state, and returns the variable value
(define m-state-assign
  (lambda (variable exp s)
    (cond
      ((state-change? exp) ;changes state 
       (m-state-assign variable (operand1 exp) (m-state exp s)))
       (else
       (add-var variable (m-value exp s) (remove-var variable s))))))

(define assignment?
  (lambda (stmt)
    (eq? '= (operator stmt))))
  
(define state-change?
  (lambda (stmt)
    (cond
      ((list? stmt) 
       (eq? '= (operator stmt)))
      (else
       #f))))

(define m-value-int
  (lambda (exp)
    (cond
      ((null? exp) 0)
      ((number? exp) exp) ; '6
      ((eq? (operator exp) '+)
       (+ (m-value-int (operand1 exp)) (m-value-int (operand2 exp))))
      ((eq? (operator exp) '-)
       (- (m-value-int (operand1 exp)) (m-value-int (operand2 exp))))
      ((eq? (operator exp) '*)
       (* (m-value-int (operand1 exp)) (m-value-int (operand2 exp))))
      ((eq? (operator exp) '%)
       (remainder (m-value-int (operand1 exp)) (m-value-int (operand2 exp))))
      ((eq? (operator exp) '/)
       (quotient (m-value-int (operand1 exp)) (m-value-int (operand2 exp))))
      (else
       (error 'badoperation "Unknown operator")))))

(define add-var
  (lambda (variable value s)
    (cons (cons variable (var-list s)) (cons (cons value (value-list s)) '()))))

(define remove-var
  (lambda (variable s)
    (cond
      ((null? (var-list s)) s)
      ((eq? (car (var-list s)) variable) (removefirstvar s))
      (else
       (add-var (car (var-list s)) (car (value-list s)) (remove-var variable (removefirstvar s)))))))


; initializes a variable and returns the value of the initialized variable



