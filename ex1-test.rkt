#lang racket/base
(require rackunit
         "compilers_ex1.rkt")



(test-begin
  (let ([tests (list
              `(+ (read) -2) ; test case: r1 is exp, r2 is int
              `(+ 1 (+ (read) (+ (read) -2))) ; test case: switch exp and int, extract int and swap with read, sum r2 int with r1
              `(+ 2 (+ (read) 2)) ; swap r1 and r2, sum r1 r2
              `(+ 2 (+ (read) (+ (+ (read) (- (read))) -4))) ; subtract works still.
              `(+ (+ (+ 5 (read)) (- (read))) -4))]) ; subtract read,, combine 5 and 4
  (check andmap R0? tests)
  (for-each
   (lambda (x)
     (let ([result (pe-arith x)])
       (check-pred R0? result)
       (check-equal? (interp-R0 result) (interp-R0 x))))
   tests)))
