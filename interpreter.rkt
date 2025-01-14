; Brian Li (bvl8) Helen Zhao (hxz347) Mark Lalor (mwl58)

; If you are using racket instead of scheme, uncomment these two lines, comment the (load "simpleParser.scm") and uncomment the (require "simpleParser.scm")
#lang racket
(require "parser.scm")
(provide (all-defined-out))

; An interpreter for the simple language that uses call/cc for the continuations.  Does not handle side effects.
;(define call/cc call-with-current-continuation)

; The functions that start interpret-...  all return the current environment.
; The functions that start eval-...  all return a value

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.  The returned value is in the environment.
(define interpret
  (lambda (file main-class)
    (scheme->language
     (eval-expression (list 'funcall (list 'dot main-class 'main))
                      (call/cc
                       (lambda (return)
                         (interpret-statement-list (parser file) (newenvironment) return break-error continue-error throw-error)))
                      throw-error))))

(define break-error
  (lambda (env)
    (myerror "Break used outside of loop")))
  
(define continue-error
  (lambda (env)
    (myerror "Continue used outside of loop")))

(define throw-error
  (lambda (v env)
    (myerror "Uncaught exception thrown")))

; interprets a list of statements.  The environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list environment return break continue throw)
    (if (null? statement-list)
        environment
        (interpret-statement-list (cdr statement-list) (interpret-statement (car statement-list) environment return break continue throw) return break continue throw))))

; interpret a statement in the environment with continuations for return, break, continue, throw
(define interpret-statement
  (lambda (statement environment return break continue throw)
    (cond
      ((eq? 'class (statement-type statement)) (interpret-class statement environment return break continue throw))
      ((eq? 'return (statement-type statement)) (interpret-return statement environment return throw))
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment throw))
      ((eq? '= (statement-type statement)) (interpret-assign statement environment throw))
      ((eq? 'if (statement-type statement)) (interpret-if statement environment return break continue throw))
      ((eq? 'while (statement-type statement)) (interpret-while statement environment return throw))
      ((eq? 'continue (statement-type statement)) (continue environment))
      ((eq? 'break (statement-type statement)) (break environment))
      ((eq? 'begin (statement-type statement)) (interpret-block statement environment return break continue throw))
      ((eq? 'throw (statement-type statement)) (interpret-throw statement environment throw))
      ((eq? 'function (statement-type statement)) (interpret-function statement (lambda (env) (lookup 'super environment)) environment return break continue throw))
      ((eq? 'try (statement-type statement)) (interpret-try statement environment return break continue throw))
      ((eq? 'funcall (statement-type statement)) (interpret-function-call statement environment return break continue throw))                                                        
      (else (myerror "Unknown statement:" (statement-type statement))))))

; Filters statements by their statement type
(define filter-sublists
  (lambda (list query)
    (cond
      ((null? list) list)
      ((eq? (statement-type (car list)) query) (cons (car list) (filter-sublists (cdr list) query)))
      (else (filter-sublists (cdr list) query)))))

; Adds the class closure to the environment
(define interpret-class
  (lambda (statement environment return break continue throw)
    (insert (class-name statement)
            (list (interpret-statement-list (filter-sublists (class-body statement) 'var)
                                            (newenvironment) return break continue throw)
                  (interpret-functions (append (filter-sublists (class-body statement) 'function)
                                               (filter-sublists (class-body statement) 'static-function))
                                       (class-name statement)
                                       (newenvironment) return break continue throw)
                  (lambda (env) (if (extends? (class-superclass statement))
                                    (lookup (superclass-name (class-superclass statement)) env)
                                    '())))
            environment)))

; Returns an environment after interpreting the function list. This function takes a class to assign an associating class closure to each function closure
(define interpret-functions
  (lambda (function-list class-name environment return break continue throw)
    (if (null? function-list)
        environment
        (interpret-functions (cdr function-list) class-name (interpret-function (car function-list) (lambda(env) (lookup class-name env)) environment return break continue throw) return break continue throw))))

; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment return throw)
    (return (eval-expression (get-expr statement) environment throw))))

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define interpret-declare
  (lambda (statement environment throw)
    (if (exists-declare-value? statement)
        (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment throw) environment)
        (insert (get-declare-var statement) 'novalue environment))))

; Updates the environment to add an new binding for a variable
(define interpret-assign
  (lambda (statement environment throw)
    (let ((assign-lhs (get-assign-lhs statement))
          (oclosure (if (exists? 'this environment)
                        (lookup 'this environment)
                        '())))
      (if (list? assign-lhs) ; should abstract as dot?
          (begin (update (dot-rhs assign-lhs)
                         (eval-expression (get-assign-rhs statement) environment throw)
                         (instance-fields (lookup (dot-lhs assign-lhs) environment)))
                 environment)
          ; Update is a side effect to update the environment/instance fields. Need to return environment in the end so begin is used
          (begin (update assign-lhs (eval-expression (get-assign-rhs statement) environment throw) (if (exists? assign-lhs environment)
                                                                                                       environment
                                                                                                       (instance-fields oclosure)))
                 environment)))))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement environment return break continue throw)
    (cond
      ((eval-expression (get-condition statement) environment throw) (interpret-statement (get-then statement) environment return break continue throw))
      ((exists-else? statement) (interpret-statement (get-else statement) environment return break continue throw))
      (else environment))))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment return throw)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body environment)
                        (if (eval-expression condition environment throw)
                            (loop condition body (interpret-statement body environment return break (lambda (env) (break (loop condition body env))) throw))
                            environment))))
         (loop (get-condition statement) (get-body statement) environment))))))

; Interprets a block.  The break, continue, and throw continuations must be adjusted to pop the environment
(define interpret-block
  (lambda (statement environment return break continue throw)
    (pop-frame (interpret-statement-list (cdr statement)
                                         (push-frame environment)
                                         return
                                         (lambda (env) (break (pop-frame env)))
                                         (lambda (env) (continue (pop-frame env)))
                                         (lambda (v env) (throw v (pop-frame env)))))))

; We use a continuation to throw the proper value. Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda (statement environment throw)
    (throw (eval-expression (get-expr statement) environment throw) environment)))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement environment return break continue throw jump finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env) (throw ex (interpret-block finally-block env return break continue throw)))) 
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
              (jump (interpret-block finally-block
                                     (pop-frame (interpret-statement-list 
                                                 (get-body catch-statement) 
                                                 (insert (catch-var catch-statement) ex (push-frame env))
                                                 return 
                                                 (lambda (env2) (break (pop-frame env2))) 
                                                 (lambda (env2) (continue (pop-frame env2))) 
                                                 (lambda (v env2) (throw v (pop-frame env2)))))
                                     return break continue throw)))))))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement environment return break continue throw)
    (call/cc
     (lambda (jump)
       (let* ((finally-block (make-finally-block (get-finally statement)))
              (try-block (make-try-block (get-try statement)))
              (new-return (lambda (v) (begin (interpret-block finally-block environment return break continue throw) (return v))))
              (new-break (lambda (env) (break (interpret-block finally-block env return break continue throw))))
              (new-continue (lambda (env) (continue (interpret-block finally-block env return break continue throw))))
              (new-throw (create-throw-catch-continuation (get-catch statement) environment return break continue throw jump finally-block)))
         (interpret-block finally-block
                          (interpret-block try-block environment new-return new-break new-continue new-throw)
                          return break continue throw))))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

; Adds a function definition to the environment and returns the result of the new state
(define interpret-function
  (lambda (statement cclosure environment return break continue throw)
    (add-function-binding statement cclosure environment)))

; Takes a function definition and adds it to the environment
(define add-function-binding
  (lambda (statement cclosure environment)
    (if (global-environment? environment)
        (insert (func-name statement) (list (param-body statement) get-global-environment-func cclosure) environment)
        (insert (func-name statement)
                (list (param-body statement) (get-snapshot-environment-func (func-name statement)) cclosure)
                environment))))

; Takes an environment and returns the global frame of the environment
(define get-global-environment-func
  (lambda (environment)
    (cond
      ((null? (cdr environment)) environment)
      (else
       (get-global-environment-func (cdr environment))))))

; Takes a function name and returns a function that maps the given environment to an updated snapshot environment
(define get-snapshot-environment-func
  (lambda (function-name)
    (lambda (environment)
      (filter-environment-by-name function-name environment))))

; Filters elements from environment until the first instance of name is matched
(define filter-environment-by-name
  (lambda (name environment)
    (cond
      ((exists-in-list? name (variables (topframe environment)))
       (cons (suffix name (topframe environment)) (remainingframes environment)))
      (else
       (filter-environment-by-name name (remainingframes environment))))))

; Returns the list when the first item of the list matches 'x
; (suffix 'x '(a b c x d)) => (x d)
(define suffix
  (lambda (x lis)
    (cond
      ((eq? (car (variables lis)) x) lis)
      (else (suffix x (list (cdr (variables lis)) (cdr (store lis))))))))

; Evaluates the expression and then returns the environment
; Uses begin because eval-expression must first modify the environment using boxes
; We return the same environment because this function is only ever used when a function is called without an assignment
(define interpret-function-call
  (lambda (statement environment return break continue throw)
    (begin
      (eval-expression statement environment throw)
      environment)))

; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define eval-expression
  (lambda (expr environment throw)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((not (list? expr)) (eval-var expr environment))
      ((eq? (statement-type expr) 'new) (eval-new (class-name expr) environment))
      ((eq? (statement-type expr) 'dot) (eval-dot expr environment throw)) ; always getting instance fields 
      ((eq? (statement-type expr) 'funcall) (eval-function (to-dot-expr expr) environment throw))
      (else (eval-operator expr environment throw)))))

; Returns the value of a variable. If the variable doesn't exist in the environment, it will look it up in the instance 'this
(define eval-var
  (lambda (var-name environment)
    (letrec ((oclosure (if (exists? 'this environment)
                           (lookup 'this environment)
                           '()))
             (cclosure (if (exists? 'super environment)
                           (lookup 'super environment)
                           '())) 
             (class-vars (if (null? cclosure)
                             '()
                             (get-class-vars cclosure environment)))
             (instance-vars (if (null? oclosure)
                                '()
                                (instance-fields oclosure))))
      (cond
        ((exists? var-name environment) (lookup var-name environment)) 
        (else
         (unbox (get-ith-val-in-env (instance-fields oclosure) (- (environment-length class-vars) (+ (environment-indexof var-name class-vars) 1)))))))))

; Recursively finds all accessible variables of a class given the class closure
(define get-class-vars
  (lambda (cclosure environment)
    (let ((cclosure-vars (cclosure-vars cclosure))
          (sclosure ((cclosure-superfun cclosure) environment)))
      (cond
        ((null? sclosure) cclosure-vars)
        (else
         (append cclosure-vars (get-class-vars sclosure environment)))))))

; Counts the number of variables in an environment
(define environment-length
  (lambda (environment)
    (cond
      ((null? environment) 0)
      (else
       (+ (length (variables (topframe environment))) (environment-length (remainingframes environment)))))))

; Finds the index of a variable in an environment
(define environment-indexof
  (lambda (var environment)
    (environment-indexof-helper var environment 0)))

; Helper function for environment-indexof
(define environment-indexof-helper
  (lambda (var environment acc)
    (cond
      ((null? environment) -1)
      ((exists-in-list? var (variables (topframe environment)))
       (+  acc (indexof var (variables (topframe environment)))))
      (else
       (environment-indexof-helper var (remainingframes environment) (+ acc (length (variables (topframe environment)))))))))

; Returns the value of a dot expression
(define eval-dot
  (lambda (expr environment throw)
    (let ((oclosure (eval-expression (dot-lhs expr) environment throw)))    
      (eval-expression (dot-rhs expr) (instance-fields oclosure) throw))))

; Returns an instance closure given class-name
(define eval-new
  (lambda (class-name environment)
    (let ((cclosure (lookup class-name environment)))
      (list (reverse-environment (get-instance-fields cclosure environment))
            cclosure))))

; Takes a class closure and returns a copy of the variables in form of an environment
(define get-instance-fields
  (lambda (cclosure environment)
    (let ((sclosure ((cclosure-superfun cclosure) environment)))
      (cond
        ((null? sclosure) (environment-shallow-copy (cclosure-vars cclosure)))
        (else
         (append (environment-shallow-copy (cclosure-vars cclosure))
                 (environment-shallow-copy (get-instance-fields sclosure environment))))))))

; Reverses each frame and reverses the order of the frames
(define reverse-environment
  (lambda (env)
    (reverse* (reverse env))))

; Reverses the list and its sublists
(define reverse*
  (lambda (l)
    (cond
      ((null? l) '())
      ((list? (car l)) (cons (reverse* (car l)) (reverse* (cdr l))))
      (else
       (append (reverse* (cdr l)) (list (car l)))))))

; Gets the ith element in the environment
(define get-ith-val-in-env
  (lambda (env i)
    (list-ref (combine-env-vals env) i)))

; Combines all the values in the environment into a single list
(define combine-env-vals
  (lambda (env)
    (cond
      ((null? env) '())
      (else (append (store (topframe env)) (combine-env-vals (remainingframes env)))))))

; Copies all the values in the environment into a new environment
(define environment-shallow-copy
  (lambda (environment)
    (cond
      ((null? environment) environment)
      (else
       (cons (frame-shallow-copy (topframe environment))
             (environment-shallow-copy (remainingframes environment)))))))

; copy class closure
(define frame-shallow-copy
  (lambda (frame)
    (cond
      ((null? (car frame)) frame)
      (else
       (add-to-frame (car (variables frame)) (lookup-in-frame (car (variables frame)) frame)
                     (frame-shallow-copy (list (cdr (variables frame)) (cdr (cadr frame)))))))))

; Converts an expression to be a dotted expression
; Pre-condition: input for expr is usually (funcall ... params)
(define to-dot-expr
  (lambda (expr)
    (if (list? (cadr expr)) 
        expr
        (append (list 'funcall (list 'dot 'this (cadr expr))) (cddr expr)))))

; Uses eval-expression on a list
(define eval-expression-list
  (lambda (expr-list environment throw)
    (if (null? expr-list)
        expr-list
        (cons (eval-expression (car expr-list) environment throw) (eval-expression-list (cdr expr-list) environment throw)))))

; Gives the value result of a function
; Pre-condition: expr always contains a dot expression
(define eval-function
  (lambda (expr environment throw)
    (call/cc
     (lambda (function-return)
       (letrec ((oclosure (if (or (eq? (dot-lhs (func-name expr)) 'super) (eq? (dot-lhs (func-name expr)) 'this))
                              (lookup 'this environment)
                              (eval-expression (dot-lhs (func-name expr)) environment throw)))
                (cclosure (cond
                            ((eq? (dot-lhs (func-name expr)) 'super) ((cclosure-superfun (lookup 'super environment)) environment))
                            ((eq? (dot-lhs (func-name expr)) 'this) (instance-class oclosure))
                            ((not (exists? 'this environment)) oclosure)
                            (else (instance-class oclosure))))
                (closure (lookup-fun (dot-rhs (func-name expr))
                                     cclosure
                                     environment)))
         (interpret-statement-list (func-body closure)
                                   (insert 'super
                                           ((func-cclosure closure) environment)
                                           (insert 'this
                                                   oclosure
                                                   (insert-all (formal-params closure)
                                                               (eval-expression-list (actual-params expr) environment throw)
                                                               (push-frame ((environment-function closure) environment)))))
                                   function-return break-error continue-error (lambda (v func-env) (throw v environment))))))))

(define lookup-fun
  (lambda (fun-name cclosure environment)
    (if (exists? fun-name environment)
        (lookup fun-name environment) ; If function is defined in the environment, use the one in the environment (hides the instance method one anyways)
        (letrec ((fun-env (cclosure-funs cclosure))
                 (lamb (cdr cclosure))
                 (lam (cddr cclosure))
                 (sclosure ((cclosure-superfun cclosure) environment)))
          (cond
            ((exists? fun-name fun-env) (lookup fun-name fun-env))
            (else
             (lookup-fun fun-name sclosure environment)))))))

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define eval-operator
  (lambda (expr environment throw)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment throw)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) environment throw)))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment throw) environment throw)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment throw)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) environment throw))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment throw)))
      (else (myerror "Unknown operator:" (operator expr))))))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))


;-----------------
; HELPER FUNCTIONS
;-----------------
(define class-name cadr)
(define class-superclass caddr)
(define class-body cadddr)
(define superclass-name cadr)
(define extends?
  (lambda (expr)
    (not (null? expr))))
(define cclosure-vars car)
(define cclosure-funs cadr)
(define cclosure-superfun caddr)
(define dot-lhs cadr)
(define dot-rhs caddr)
(define instance-fields car)
(define instance-class cadr)
    
; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

(define global-environment?
  (lambda (environment)
    (null? (cdr environment))))

; these helper functions define the parts of the various statement types
(define statement-type operator)
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3); abstractions for getting function name and closure from a statement 
(define param-body cddr)
(define formal-params caar)
(define environment-function cadr)
(define func-name cadr)
(define func-cclosure caddr)
(define func-body
  (lambda (closure)
    (cadr (car closure))))

; get the actual params for funcall
(define actual-params cddr)

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))


;------------------------
; Environment/State Functions
;------------------------
      
; create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe))))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda ()
    '(() ())))

; add a frame onto the top of the environment
(define push-frame
  (lambda (environment)
    (cons (newframe) environment)))

; remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (cdr environment)))

; some abstractions
(define topframe car)
(define remainingframes cdr)

; does a variable exist in the environment?
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var environment)
    (lookup-variable var environment)))
  
; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? 'novalue value)
          (myerror "error: variable without an assigned value:" var)
          value))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      (else (lookup-in-env var (cdr environment))))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (myerror "error: undefined variable" var))
      (else (language->scheme (unbox (get-value (indexof var (variables frame)) (store frame))))))))

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (car l))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

; Uses insert function to insert a list of variables and values
(define insert-all
  (lambda (var-list val-list environment)
    (if (null? var-list)
        environment
        (insert-all (cdr var-list) (cdr val-list) (insert (car var-list) (car val-list) environment)))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (update-existing var val environment)
        (myerror "error: variable used but not defined:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (box (scheme->language val)) (store frame)))))

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (car varlist))
       (begin
         (set-box! (car vallist) (scheme->language val))
         vallist))
      (else (cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))

; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))

; Functions to convert the Scheme #t and #f to our languages true and false, and back.

(define language->scheme
  (lambda (v) 
    (cond 
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))

; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))

;(interpret "test/part4/0" 'A)
;(interpret "test/part4/1" 'A)
;(interpret "test/part4/2" 'A)
;(interpret "test/part4/3" 'A)
;(interpret "test/part4/4" 'A)
;(interpret "test/part4/5" 'A)
;(interpret "test/part4/6" 'A)
;(interpret "test/part4/7" 'C)
;(interpret "test/part4/8" 'Square)
;(interpret "test/part4/9" 'Square)
;(interpret "test/part4/10" 'List)
;(interpret "test/part4/11" 'List)
;(interpret "test/part4/12" 'List)
;(interpret "test/part4/13" 'C)
