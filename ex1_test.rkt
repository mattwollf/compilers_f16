#lang racket/base
(require rackunit
         "compilers_ex1.rkt")

(test-begin
  (let ([tests (list
              `(+ (read) -2)
              `(+ 1 (+ (read) (+ (read) -2)))
              `(+ 2 (+ (read) 2))
              `(+ 2 (+ (read) (+ (+ (read) (- (read))) -4)))
              `(+ (+ (+ 5 (read)) (- (read))) -4))])
  (check andmap R0? tests)
  (for-each
   (lambda (x)
     (let ([result (pe-arith x)])
       (check-pred R0? result)
       (check-equal? (interp-R0 result) (interp-R0 x))))
   tests)))
