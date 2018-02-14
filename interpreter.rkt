#!/usr/bin/racket
#lang racket

(provide (all-defined-out))

(require "simpleParser.scm")

; Just grabbed the expected values with regex to test the tester program. Errors need some changes depending on our implementation.
(define interpret-test-test
  (lambda (filename)
    (cond
      ((eq? filename "test/programs/1") 150)
      ((eq? filename "test/programs/10") -39)
      ((eq? filename "test/programs/11") (error "error"))
      ((eq? filename "test/programs/12") (error "error"))
      ((eq? filename "test/programs/13") (error "error"))
      ((eq? filename "test/programs/14") (error "error"))
      ((eq? filename "test/programs/15") "true")
      ((eq? filename "test/programs/16") 100)
      ((eq? filename "test/programs/17") "false")
      ((eq? filename "test/programs/18") "true")
      ((eq? filename "test/programs/19") 128)
      ((eq? filename "test/programs/2") -4)
      ((eq? filename "test/programs/20") 12)
      ((eq? filename "test/programs/21") 30)
      ((eq? filename "test/programs/22") 11)
      ((eq? filename "test/programs/23") 1106)
      ((eq? filename "test/programs/24") 12)
      ((eq? filename "test/programs/25") 16)
      ((eq? filename "test/programs/26") 72)
      ((eq? filename "test/programs/27") 21)
      ((eq? filename "test/programs/28") 164)
      ((eq? filename "test/programs/3") 10)
      ((eq? filename "test/programs/4") 16)
      ((eq? filename "test/programs/5") 220)
      ((eq? filename "test/programs/6") 5)
      ((eq? filename "test/programs/7") 6)
      ((eq? filename "test/programs/8") 10)
      ((eq? filename "test/programs/9") 5)
      (else (error "error")))))

; Interpret a file by calling our interpreter with the output of the parser and with an empty state.
(define interpret
  (lambda (filename)
    (interpret-parsed (parser filename) '('() '()))))

; Abstractions for our grammar / working with parsed data.

(define current-statement car)
(define rest-statements cdr)

(define operator car)

; variable declaration: (var variable) or (var variable value)
(define declaration-variable cadr)
(define declaration-value caddr)

; assignment: (= variable expression)
(define assignment-variable cadr)
(define assignment-expression caddr)

; return: (return expression)
(define return-expression cadr)

; if statement (if conditional then-statement optional-else-statement)
(define if-conditional cadr)
(define if-then-statement caddr)
(define if-else-statement cadddr)

; while statement: (while conditional body-statement)
(define while-conditional cadr)
(define while-body-statement caddr)

; Other expressions...

; +, -, *, /
(define binaryop-first cadr)
(define binaryop-second caddr)

; TODO
; Get the value that 'return' is mapped to in the state (ref. "Returning a Value" in instructions)
(define get-return
  (lambda (state)
    '()))

; TODO/Question
; Do we need some other state mapping for "M-boolean" and stuff... will find out soon!
(define interpret-parsed
  (lambda (parse-lis state)
    (cond
      ((null? parse-lis) (get-return state))
      (else (interpret-parsed (rest-statements parse-lis) (M-state (current-statement parse-lis) state))))))

(define M-state
  (lambda (item state)
    (cond
      ((eq? (operator item) 'var) 1) ; handle changing state for variable declaration...
      ((eq? (operator item) '=) 1)   ; handle changing state for assignment, etc...
      ((eq? (operator item) 'return) 1)
      ((eq? (operator item) 'if) 1)
      ((eq? (operator item) 'while) 1)
      (else (error (operator item))))))
    