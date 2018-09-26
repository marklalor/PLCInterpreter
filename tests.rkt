#!/usr/bin/racket
#lang racket

(require "interpreter.rkt")
(require rackunit)



(define test-program
  (lambda (category id main-class expected)
    (check-eq? (interpret (string-join (list "test/" category "/" id) "") main-class) expected)))

(test-program "objects" "0" 'A 0)
(test-program "objects" "1" 'A 15)
(test-program "objects" "2" 'A 12)
(test-program "objects" "3" 'A 125)
(test-program "objects" "4" 'A 36)
(test-program "objects" "5" 'A 54)
(test-program "objects" "6" 'A 110)
(test-program "objects" "7" 'C 26)
(test-program "objects" "8" 'Square 117)
(test-program "objects" "9" 'Square 32)
(test-program "objects" "10" 'List 15)
(test-program "objects" "11" 'List 123456)
(test-program "objects" "12" 'List 5285)
(test-program "objects" "13" 'C -516)

"All tests complete."
