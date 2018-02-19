#!/usr/bin/racket
#lang racket

(require "interpreter.scm")
(require rackunit)

(provide (all-defined-out))

(check-eq? (interpret "test/programs/1") 150)
(check-eq? (interpret "test/programs/2") -4)
(check-eq? (interpret "test/programs/3") 10)
(check-eq? (interpret "test/programs/4") 16)
(check-eq? (interpret "test/programs/5") 220)
(check-eq? (interpret "test/programs/6") 5)
(check-eq? (interpret "test/programs/7") 6)
(check-eq? (interpret "test/programs/8") 10)
(check-eq? (interpret "test/programs/9") 5)
(check-eq? (interpret "test/programs/10") -39)
(check-equal? (interpret "test/programs/11") "error: UndeclaredVariable")
(check-equal? (interpret "test/programs/12") "error: UndeclaredVariable")
(check-equal? (interpret "test/programs/13") "error: UnassignedVariable")
(check-equal? (interpret "test/programs/14") "error: RedefiningVariable")
(check-eq? (interpret "test/programs/15") 'true)
(check-eq? (interpret "test/programs/16") 100)
(check-eq? (interpret "test/programs/17") 'false)
(check-eq? (interpret "test/programs/18") 'true)
(check-eq? (interpret "test/programs/19") 128)
(check-eq? (interpret "test/programs/20") 12)
(check-eq? (interpret "test/programs/21") 30)
(check-eq? (interpret "test/programs/22") 11)
(check-eq? (interpret "test/programs/23") 1106)
(check-eq? (interpret "test/programs/24") 12)
(check-eq? (interpret "test/programs/25") 16)
(check-eq? (interpret "test/programs/26") 72)
(check-eq? (interpret "test/programs/27") 21)
(check-eq? (interpret "test/programs/28") 164)

"Tests complete."