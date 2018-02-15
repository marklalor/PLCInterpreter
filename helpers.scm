; remove the first variable and value from the state
(define removefirstvar
  (lambda (s)
    (cons (cdr (var-list s)) (cons (cdr (value-list s)) '()))))

