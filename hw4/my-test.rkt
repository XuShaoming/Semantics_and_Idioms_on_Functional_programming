#lang racket

(require "hw4.rkt")

(define seq-test1 (sequence 3 11 2))
(define seq-test2 (sequence 3 8 3))
(define seq-test3 (sequence 3 2 1))
(define xs (list (list 1 2)  4 (list 5 6)))
(define test4 (cached-assoc xs 10))
seq-test1
seq-test2
seq-test3
(stream-for-n-steps ones 5)
(test4 1)

(define a 2)
(while-less 7 do (begin (set! a (+ a 1)) (print "x") a))
a
(while-less 7 do (begin (set! a (+ a 1)) (print "x") a))
a