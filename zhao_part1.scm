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
      ((null? stmt) s)
      ((declareandassign? stmt)
       (m-state-assign (assignment-variable stmt) (assignment-expression stmt) (m-state-declare variable s)))
      ((declaration? stmt) (m-state-declare (declaration-variable stmt) s))
      ((assignment? stmt) (m-state-assign (assignment-variable stmt) (assignment-expression stmt) s))
      ; if
      ; while
      ((return? stmt) (m-state-return (return-expression stmt) s)))))

(define m-state-return
  (lambda (stmt s)
    (m-state-assign 'return (return-expression stmt) s)))

(define m-value-exp-value car)
(define m-value-exp-state cadr)

(define m-value-exp
  (lambda (exp s)
    (cond
      ((number? exp) '((m-value-int exp) s))
      ((list? exp) '((m-value-int exp) s))
      ((assignment? exp) '((m-value (assignment-variable exp) (m-state exp s)) (m-state exp s)))
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

; Statement checks

(define assignment?
  (lambda (stmt))
    (eq? '= (operator stmt)))

(define declaration?
  (lambda (stmt)
    (and (null? (cddr stmt)) (eq? (car stmt) 'var))))

(define declarationandassign?
  (lambda (stmt)
    (and (not (null? (cddr stmt))) (eq? (car stmt) 'var))))

(define return?
  (lambda (stmt)
    (eq? 'return (car stmt))))

; State functions

; assigns variable to the value of exp, updates the variable in the state, and returns the variable value
(define m-state-assign
  (lambda (variable exp s)
    (cond
      ((state-change? exp) ;changes state 
       (m-state-assign variable (m-value-exp-value (m-value-exp exp s)) (m-value-exp-state (m-value-exp exp s))))
       (else
       (add-var variable (m-value exp s) (remove-var variable s))))))

  
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



