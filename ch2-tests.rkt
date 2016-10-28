#lang racket/base
(require rackunit
         chk
         "ch2_exercises.rkt"
         "utilities.rkt")

(chk #:x (lookup 'x '()) "couldnt_match_symbol")

(test-begin
 (let ([tests (list
               (list (cons 'x 5))
               (list (cons 'y 6) (cons 'x 5))
               (list (cons 'y 6) (cons 'x 5) (cons 'x 6))
               (list (cons 'y 6) (cons 'z 1) (cons 'x 5) (cons 'a 3))
               )])
   
   (for-each
    (lambda (e)
      (check-equal? (lookup 'x e) 5))
    tests)))

(define tests (list
               `(program (let ([x 32]) (+ (let ([x 10]) x) x))) ;; first provided example from text
               `(program (let ([x (let ([x 4]) (+ x 1))]) (+ x 2))) ;; second provided example from test should yield
               `(program (let ([x (let ([x (let ([x 42]) (+ x 1))]) (+ x -1))]) x)) ;; invented nest within nest times 4
               `(program (let ([x (+ 12 20)]) (+ x 10)))
               `(program (let ([x 32]) (+ (let ([x 10]) x) x)))
               `(program (let ([x1 32]) (+ (let ([x2 10]) x2) x1)))
               `(program (let ([x (read)]) (let ([y (read)]) (+ x (- y)))))
               `(program (+ 52 (- 10)))
               `(program (let ([x (+ (- 10) 11)]) (+ x 41)))
               `(program (let ([a 42]) (let ([b a]) b)))
               '(program (let ([v 1]) (let ([w 46]) (let ([x (+ v 7)]) (let ([y (+ 4 w)]) (let ([z (+ x w)]) (+ z (- y))))))))
                                                          
               )
  )

(test-begin
 (for-each
  (lambda (x)
    (let ([uniquified ((uniquify null) x)]
          [interpreter (interp-R1 null)])
      (check-equal? (interpreter uniquified) (interpreter x))))
  tests))

(test-begin
 (for-each
  (lambda (x)
    (let ([interpreter (interp-R1 null)]
         )
      (check-equal? (interpreter x) (string->number (compile-run x compile-R1)))))
  tests))