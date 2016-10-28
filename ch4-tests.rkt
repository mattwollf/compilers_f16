#lang racket/base

(require "ch4-exercises.rkt"
         rackunit "utilities.rkt"
         )

(define big-bool-test '(program (and #t (if (not (> 2 (+ 2 4))) (or #t #t) (or #t #f)))))
(define big-flatten-test '(program (if (eq? (read) 0) (let ([x 777]) x) (+ 2 (if (eq? (read) 0) 40 444)))))
(define type-tests-integer
  (list
   '(program (if #t 1 2))
   '(program (let ([x (- 1)]) (+ 43 x)))
   '(program (if (> 5 2) (let ([x 1]) 1) 2))
   ))

(define type-tests-boolean
  (list
    '(program #f)
    '(program (not (if (< 5 2) #f #t)))
    `(program (and (or #t #f) (if (> 5 2) #t #t)))
    ))

(define type-tests-failing
  (list
   '(program (let ([x (if (<= 2 #t) 5 2)]) x))
   '(program (let ([x (and #t 5)]) x))
   '(program (let ([x (+ 5 #t)]) x))
   '(program (let ([x (not 5)]) x))
   '(program (let ([x (or 5 #t)]) x))
    ))

(test-begin
  (for-each
    (lambda (x)
      (check-equal? (cadadr ((typecheck-R2 '()) x)) 'Integer))
    type-tests-integer))

(test-begin
  (for-each
    (lambda (x)
      (check-equal? (cadadr ((typecheck-R2 '()) x)) 'Boolean))
    type-tests-boolean))

(test-begin
  (for-each
    (lambda (x)
      (check-exn exn:fail? (lambda () (((typecheck-R2 '()) x)))))
      type-tests-failing))

(for-each
  (lambda (x)
    ((typecheck-R2 '()) x)) type-tests-boolean)

(test-case
  "simple if flatten test - one variable generated, if statement pattern matched"
  (let* ([input '(program (if #f 0 42))]
         [output ((flatten-R2) input)])
    (check = 1 (length (cadr output)))
    (check-match (cddr output) (list `(if (eq? #t #f) ((assign ,rc 0)) ((assign ,rc 42)))  `(return ,rc)))
    ))

(test-case
  "simple not flatten test"
  (let* ([input '(program (not #f))]
         [output ((flatten-R2) input)])
    (check = 1 (length (cadr output)))
    (check-match (cddr output) (list `(assign ,var (not #f)) `(return ,var)))
    ))

(test-case
  "simple and flatten"
  (let* ([input '(program (and (not #f) #t))]
         [output ((flatten-R2) input)])
    (check = 2 (length (cadr output)))
    (check-match (cddr output) (list `(assign ,tmp (not #f)) `(if (eq? #t ,tmp) ((assign ,rc (eq? #t #t))) ((assign ,rc #f))) `(return ,rc)))
    ))

#|
(define si-test-cases
  (list
    (cons 

(test-case
  "select-instruction boolean tests"
  |#
