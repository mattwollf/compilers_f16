#lang racket

(define (lookup e env)
  (cond ((null? env) (error 'lookup "couldnt_match_symbol" e))
        ((eq? e (car (car env))) (cdr (car env)))
        (else (lookup e (cdr env)))))

(provide lookup)