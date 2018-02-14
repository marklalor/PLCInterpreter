(require "simpleParser.scm")

(define interpret
  (lambda (filename)
    (interpret-parsed (parser filename))))

(define operator (lambda statement) (car statement))

(define interpret-parsed
  (lambda (parse-lis)
    (cond
      ((eq? (operator parse-lis) 'return) 
    
