#lang racket/base

(require racket/string
         racket/system
         racket/port
         )

(define p1
  `(program (let ([x 32]) (+ (let ([x2 10]) x2) x))))
(define p0
  `(program (+ 52 (- 10))))

(define (lookup e env)
  (cond ((null? env) (error 'lookup "couldnt_match_symbol" e))
        ((eq? e (car (car env))) (cdr (car env)))
        (else (lookup e (cdr env)))))

(define (compile-run prog compile)
  (let*
      ([filename (string-append (symbol->string (gensym 'tmp)) ".s")]
       ;[ofile (open-output-file filename #:exists 'truncate)]
       [build-cmd (string-join `("gcc" ,filename "runtime.c") " ")]
       [compiled-result (open-output-string)]
       )
    (with-output-to-file filename (λ () (display (compile prog))))
    (system build-cmd)
    (with-output-to-string (λ () (system "./a.out")))))


(provide lookup
         compile-run
         p1 p0)