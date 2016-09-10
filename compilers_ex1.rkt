#lang racket/base
(require racket/fixnum)
(require racket/match)

;;1.1
;;(+ (read) (- 8))

;;1.3
(define ast1.4 `(- 8))
(define ast1.1 `(+ (read) ,ast1.4))

;;1.4

(define (test-match ast)
  (match ast
    [`(,op ,child1 ,child2)
     (print op) (newline)
     (print child1) (newline)
     (print child2)]))

;;(test-match ast1.1)

(define (leaf? arith)
  (match arith
    [(? fixnum?) #t]
    [`(read) #t]
    [`(- ,c1) #f]
    [`(+ ,c1 ,c2) #f]))

;;(leaf? `(read))
;;(leaf? `(- 8))
;;(leaf? `(+ (read) (- 8)))

;; 1.5
(define (R0? sexp)
  (match sexp
    [(? fixnum?) #t]
    [`(read) #t]
    [`(- ,e) (R0? e)]
    [`(+ ,e1 ,e2)
     (and (R0? e1) (R0? e2))]
    [`(program ,e) (R0? e)]
    [else #f]))

;; (R0? `(+ (read) (- 8)))
;; (R0? `(- (read) (+ 8)))

;; 1.6
(define (interp-R0 e)
  (match e
    [(? fixnum?) e]
    [`(read)
     (let ([r (read)])
       (cond [(fixnum? r) r]
             [else (error 'interp-R0 "input not an integer" r)]))]
    [`(- ,(app interp-R0 v))
     (fx- 0 v)]
    [`(+ ,(app interp-R0 v1) ,(app interp-R0 v2))
     (fx+ v1 v2)]
    [`(program ,(app interp-R0 v)) v]
    ))

;; 1.7

(define (assert msg bool)
  (if bool
      bool
      ((print msg) (newline))))

(define (test-pe p)
  (assert "testing pe-arith"
          (equal? (interp-R0 p) (interp-R0 (pe-arith p)))))

(define (pe-neg r)
  (cond [(fixnum? r) (fx- 0 r)]
        [else `(- ,r)]))

(define (collapsible? e)
  (and (residual? e) (not (exp? e))))

(define (pe-add r1 r2)
  (cond [(and (fixnum? r1) (fixnum? r2)) (fx+ r1 r2)] ; reduce by summing ints
        [(and (fixnum? r1) (exp? r2))
         `(+ ,r1 ,r2)] ; this is a residual but not an exp. acceptable to return as is.
        [(and (fixnum? r1) (residual? r2))
         (pe-add (fx+ r1 (extract-residual-num r2)) (extract-residual-exp r2))] ; if r2 has an int in its r1 value, extract it and add it to r1, return r1+r2.r1, r2.r2
        [(and (fixnum? r2) (or (exp? r1) (residual? r1)))
         (pe-add r2 r1)] ; if r2 is fixnum and r1 is exp, swap values to put in proper residual form
        [(and (exp? r1) (collapsible? r2))
         (pe-add r2 r1)] ; swap and let next recursive call extract
        [(and (collapsible? r1) (exp? r2))
         (pe-add (extract-residual-num r1) (pe-add (extract-residual-exp r1) r2))]
        [(and (collapsible? r1) (collapsible? r2))
         (pe-add (pe-add (extract-residual-num r1) (extract-residual-num r2)) (pe-add (extract-residual-exp r1) (extract-residual-exp r2)))]
        [else `(+ ,r1 ,r2)]))

(define (pe-arith e)
  (match e
    [(? fixnum?) e]
    [`(read) `(read)]
    [`(- ,(app pe-arith r1))
     (pe-neg r1)]
    [`(+ ,(app pe-arith r1) ,(app pe-arith r2))
     (pe-add r1 r2)]))

(define (extract-residual-num e)
  (match e
    [(? fixnum?) e]
    [`(+ ,c1 ,c2)
     (cond [(fixnum? c1) c1]
           [(fixnum? c2) c2]
           [else 0])]))

(define (extract-residual-exp e)
  (match e
    [`(+ ,c1 ,c2)
     (cond [(exp? c2) c2]
           [(exp? c1) c1])]
    [else (error "bad input expression")]))

(define (exp? e)
  (match e
    [`(read) #t]
    [`(- (read)) #t]
    [`(+ ,c1 ,c2)
     (and (exp? c1) (exp? c2))]
    [else #f]))    

(define (residual? e)
  (match e
    [(? fixnum?) #t]
    [`(+ ,c1 ,c2)
     (and (fixnum? c1) (exp? c2))]
    [else (exp? e)]))

;;(test-pe `(+ (read) (- (+ 5 3))))
;;(test-pe `(+ 1 (+ (read) 1)))
;;(test-pe `(- (+ (read) (- 5))))

(provide pe-add
         interp-R0
         pe-arith
         R0?)
