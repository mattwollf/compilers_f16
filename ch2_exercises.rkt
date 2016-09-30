#lang racket/base

(require racket/fixnum
         racket/match
         racket/list
         racket/trace
         "utilities.rkt"
         )


(define (interp-R1 env)
  (lambda (e)
    (define recur (interp-R1 env))
    (match e
      [(? symbol?) (lookup e env)]
      [`(let ([,x ,(app recur v)]) ,body)
       (define new-env (cons (cons x v) env))
       ((interp-R1 new-env) body)]
      [(? fixnum?) e]
      [`(read)
       (define r (read))
       (cond [(fixnum? r) r]
             [else (error 'interp-R1 "expected_an_integer" r)])]
      [`(- ,(app recur v))
       (fx- 0 v)]
      [`(+ ,(app recur v1) ,(app recur v2))
       (fx+ v1 v2)]
      [`(program ,e) ((interp-R1 '()) e)]
      )))

;(define (interp-C0 env)
;  (lambda (e)
;    (match e
;      [`(program ,vlist ,alist)
;       ((interp-C0 '()) alist)]
;      [`(assign ,var ,stmt)
;       (match stmt
;         [(? integer?)
;          (define new-env (cons (cons var stmt) env))
;          ((interp-C0 new-env) (cdr alist))]
;         [`(read)
;          (define r (read))
;          (cond [(fixnum? r)
;                 (define new-env (cons (cons var stmt) env))
;                 ((interp-C0 new-env) (cdr alist))]
;                [else (error 'interp-C0 "expected_an_integer" r)])]
;         [`(- ,e1)    


(define (uniquify alist)
  (lambda (e)
    (match e
      [(? symbol?) (lookup e alist)]
      [(? integer?) e]
      [`(let ([,x ,e]) ,body)
       (define new-alist (cons (cons x (gensym x)) alist))
       `(let ([,(cdr (car new-alist)) ,((uniquify alist) e)]) ,((uniquify new-alist) body))]
      [`(program ,e)
       `(program ,((uniquify alist) e))]
      [`(,op ,es ...)
       `(,op ,@(map (uniquify alist) es))]
      )))

(define p1 
  `(program (let ([x 32]) (+ (let ([x2 10]) x2) x))))
(define p0
  `(program (+ 52 (- 10))))


(define (varlist alist)
  (map second alist))

(define (flatten vals)
  (match-define (list exp alist) vals)
  (lambda (e)
    (match e
      [(? integer?) (list e alist)]
      [(? symbol?) (list e alist)]
      [`(read)
       (let ([new-var (gensym 'tmp)])
         (list new-var (append alist `((assign ,new-var (read))))))]
      [`(let ([,x ,e]) ,body)
       (match-define (list ex alist-new) ((flatten (list exp alist)) e))
       ((flatten (list x (append alist-new `((assign ,x ,ex))))) body)]
      [`(program ,e)
       (match-define (list expr alist-new) ((flatten (list exp alist)) e))
       (let ([vlist (varlist alist-new)])
         (list* 'program vlist (append alist-new `((return ,(last vlist))))))]
      [`(- ,e)
       (match-define (list ex alist-new) ((flatten (list exp alist)) e))
       (let ([new-var (gensym 'tmp)])
         (let ([new-exp `((assign ,new-var (- ,ex)))])
           (list new-var (append alist-new new-exp))))]
      [`(+ ,e1 ,e2)
       (match-define (list lhs-exp lhs-al) ((flatten (list null alist)) e1))
       (match-define (list rhs-exp rhs-al) ((flatten (list lhs-exp lhs-al)) e2))
       (let ([new-var (gensym 'tmp)])
         (let ([new-exp `((assign ,new-var (+ ,lhs-exp ,rhs-exp)))])
           (list new-var (append rhs-al new-exp))))])))

(define (select-instructions alist)
  (define si (lambda (alist asm)
               (match (car alist)
                 [`(assign ,lhs ,rhs)
                  (match rhs
                    [(? integer? e)
                     (si (cdr alist)
                         (cons `(movq (int ,e) (var ,lhs)) asm))]
                    [(? symbol? e)
                     (si (cdr alist)
                         (cons `(movq (var ,e) (var ,lhs)) asm))]
                    ; if one addition operand matches lhs, we can skip the movq
                    [(or `(+ ,lhs ,(? integer? e))
                         `(+ ,(? integer? e) ,lhs))
                     (si (cdr alist)
                         (cons `(addq (int ,e) (var ,lhs)) asm))]
                    ; same as prior case but with another variable instead of int - this also matches (+ lhs lhs)
                    [(or `(+ ,(? symbol? e) ,lhs)
                         `(+ ,lhs ,(? symbol? e)))
                     (si (cdr alist)
                         (cons `(addq (var ,e) (var ,lhs)) asm))]
                    ; if one is symbol, one is int
                    [(or `(+ ,(? symbol? e) ,(? integer? i))
                         `(+ ,(? integer? i) ,(? symbol? e))) 
                     (si (cdr alist)
                         (cons `(addq (int ,i) (var ,lhs))
                               (cons `(movq (var ,e) (var ,lhs)) asm)))]
                    ; both integers
                    [`(+ ,(? integer? e1) ,(? integer? e2))
                     (si (cdr alist)
                         (cons `(addq (int ,e1) (var ,lhs))
                               (cons `(movq (int ,e2) (var ,lhs)) asm)))]
                    ;both symbols. can this somehow be combined into prior?
                    [`(+ ,(? symbol? e1) ,(? symbol? e2))
                     (si (cdr alist)
                         (cons `(addq (var ,e1) (var ,lhs))
                               (cons `(movq (var ,e2 (var ,lhs))) asm)))]
                    [`(read)
                     (si (cdr alist)
                         (cons `(movq (reg rax) (var ,lhs))
                               (cons `(callq read_int) asm)))]
                    [`(- ,(? integer? e))
                     (si (cdr alist)
                         (cons `(negq (var ,lhs)) (cons `(movq (int ,e) (var ,lhs)) asm)))]
                    
                    )]
                 
                 [`(return ,ret)
                  (cons `(movq (var ,ret) (reg rax)) asm)])))


(reverse (si alist '())))

(provide uniquify
         interp-R1
         flatten
         select-instructions)