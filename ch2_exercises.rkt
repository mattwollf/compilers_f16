#lang racket/base

(require racket/fixnum
         racket/match
         racket/list
         racket/string
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


(define (uniquify [alist null])
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

(define (varlist alist)
  (remove-duplicates (map second alist)))

(define (flatten-R1 [vals (list null null)])
  (match-define (list exp alist) vals)
  (lambda (e)
    (match e
      [(? integer?) (list e alist)]
      [(? symbol?) (list e alist)]
      [`(read)
       (let ([new-var (gensym 'tmp)])
         (list new-var (append alist `((assign ,new-var (read))))))]
      ;added this case because handling the extra assignment in previous case was causing contract violations.
      ;consider trying rewriting flatten.
      [(or `(let ([,x ,(? integer? e)]) ,body)
           `(let ([,x ,(? symbol? e)]) ,body))
       ((flatten-R1 (list x (append alist `((assign ,x ,e))))) body)]
      ;in this case e is probably an s-expression requiring flattening.
      ;we can remove the last assignment and replace it with the let variable.
      [`(let ([,x ,e]) ,body)
       (match-define (list ex alist-new) ((flatten-R1 (list exp alist)) e))
       ((flatten-R1 (list
                     x
                     (append
                      (reverse (cdr (reverse alist-new)))
                      (list (match (last alist-new)
                              [`(assign ,_ ,rhs)
                               `(assign ,x ,rhs)]))))) body)]
      [`(program ,e)
       (match-define (list expr alist-new) ((flatten-R1 (list exp alist)) e))
       (let ([vlist (varlist alist-new)])
         (list* 'program vlist (append alist-new `((return ,(last vlist))))))]
      [`(- ,e)
       (match-define (list ex alist-new) ((flatten-R1 (list exp alist)) e))
       (let ([new-var (gensym 'tmp)])
         (let ([new-exp `((assign ,new-var (- ,ex)))])
           (list new-var (append alist-new new-exp))))]
      [`(+ ,e1 ,e2)
       (match-define (list lhs-exp lhs-al) ((flatten-R1 (list null alist)) e1))
       (match-define (list rhs-exp rhs-al) ((flatten-R1 (list lhs-exp lhs-al)) e2))
       (let ([new-var (gensym 'tmp)])
         (let ([new-exp `((assign ,new-var (+ ,lhs-exp ,rhs-exp)))])
           (list new-var (append rhs-al new-exp))))])))

(define (select-instructions prog)
  (define si (lambda (alist asm)
               (match (car alist)
                 ; basic integer assignment
                 [`(assign ,lhs ,(? integer? e))
                  (si (cdr alist)
                      (cons `(movq (int ,e) (var ,lhs)) asm))]
                 ;symbol assignment
                 [`(assign ,lhs ,(? symbol? e))
                  (si (cdr alist)
                      (cons `(movq (var ,e) (var ,lhs)) asm))]
                 ; addition asignment, one operand matching the assignee
                 [(or `(assign ,lhs (+ ,lhs ,(? integer? e)))
                      `(assign ,lhs (+ ,(? integer? e) ,lhs)))
                  (si (cdr alist)
                      (cons `(addq (int ,e) (var ,lhs)) asm))]
                 ; same as above for symbols
                 [(or `(assign ,lhs (+ ,lhs ,(? symbol? e)))
                      `(assign ,lhs (+ ,(? symbol? e) ,lhs)))
                  (si (cdr alist)
                      (cons `(addq (var ,e) (var ,lhs)) asm))]
                 ; addition assignment with no matching variables
                 [(or `(assign ,lhs (+ ,(? symbol? e) ,(? integer? i)))
                      `(assign ,lhs (+ ,(? integer? i) ,(? symbol? e))))
                  (si (cdr alist)
                      (cons `(addq (int ,i) (var ,lhs))
                            (cons `(movq (var ,e) (var ,lhs)) asm)))]
                 ; addition with two integers
                 [`(assign ,lhs (+ ,(? integer? e1) ,(? integer? e2)))
                  (si (cdr alist)
                      (cons `(addq (int ,e1) (var ,lhs))
                            (cons `(movq (int ,e2) (var ,lhs)) asm)))]
                 ; addition with two symbols
                 ; TODO figire out if these can be combined, only difference is var/int
                 [`(assign ,lhs (+ ,(? symbol? e1) ,(? symbol? e2)))
                  (si (cdr alist)
                      (cons `(addq (var ,e1) (var ,lhs))
                            (cons `(movq (var ,e2) (var ,lhs)) asm)))]
                 ; read assignment
                 [`(assign ,lhs (read))
                  (si (cdr alist)
                      (cons `(movq (reg rax) (var ,lhs))
                            (cons `(callq read_int) asm)))]
                 ; negation of assignee
                 [`(assign ,lhs (- ,lhs))
                  (si (cdr alist)
                      (cons `(negq (var ,lhs)) asm))]
                 ; negation assignment
                 [`(assign ,lhs (- ,(? integer? e)))
                  (si (cdr alist)
                      (cons `(negq (var ,lhs)) (cons `(movq (int ,e) (var ,lhs)) asm)))]
                 ; symbol negation assignment
                 ; TODO figure out if we can combine these. they are the same except for the var/int in the movq
                 [`(assign ,lhs (- ,(? symbol? e)))
                  (si (cdr alist)
                      (cons `(negq (var ,lhs)) (cons `(movq (var ,e) (var ,lhs)) asm)))]
                 [`(return ,ret)
                  (cons `(movq (var ,ret) (reg rax)) asm)])))
  (list* 'program (cadr prog) (reverse (si (cddr prog) '()))))

(define (make-homes prog)
  (match-define `(program ,vars  ,instrs ...) prog)
  (define (mh offset vars ret)
    (cond [(null? vars) ret]
          [(mh (+ offset 8) (cdr vars) (cons (cons (car vars) `(deref rbp ,(- offset))) ret))]))
  (mh 8 (reverse vars) '()))

(define (assign-homes prog [homes (make-homes prog)])
  (define (non-var? e)
    (and
     (symbol? e)
     (not (eq? e 'var))))
  (define (ah instrs ret)
    (cond ((null? instrs) ret)
          (else
           (match (car instrs)
             [`(,instr (var ,v1) (var ,v2))
              (ah (cdr instrs)
                  (cons `(,instr ,(lookup v1 homes) ,(lookup v2 homes)) ret))]
             [`(,instr (var ,(? symbol? v)) (,(? non-var? e) ,i))
              (ah (cdr instrs)
                  (cons `(,instr ,(lookup v homes) (,e ,i)) ret))]
             [`(,instr (,(? non-var? e) ,i) (var ,v))
              (ah (cdr instrs)
                  (cons `(,instr (,e ,i) ,(lookup v homes)) ret))]
             [`(,instr (var ,v))
              (ah (cdr instrs)
                  (cons `(,instr ,(lookup v homes)) ret))]
             [`(callq ,func)
              (ah (cdr instrs)
                  (cons `(callq ,func) ret))]
             ))))
  (list*
   'program
   ; book notes that Mac OSX requires frame size to be a multiple of 16. is this a requirement?
   (* 8 (length (cadr prog)))
   (ah (reverse (cddr prog)) '())))

(define (patch-instructions prog)
  (define (flatten lst)
    (foldr
     (lambda (x prev) (match x
                        [(list a b)
                         #:when (and (list? a) (list? b))
                         (list* a b prev)]
                        [_ (cons x prev)]))
     '()
     lst))
  (define pi (match-lambda
               [`(,instr (deref ,reg ,offset) (deref ,reg2 ,offset2))
                (list `(movq (deref ,reg ,offset) (reg rax)) `(,instr (reg rax) (deref ,reg2 ,offset2)))]
               [a a]))
  (list*
   'program
   (cadr prog)
   (flatten (map pi (cddr prog)))))

(define (append-print prog)
  (list* 'program (cadr prog) (append (cddr prog) '((movq (reg rax) (reg rdi)) (callq print_int)))))

(define (print-x86 prog)
  (define (fmt instr)
    (format (string-append "        ~a " ) instr))
  (define p86
    (match-lambda
      [(? symbol? e) (symbol->string e)]
      [`(reg ,e)
       (format "%~a" e)]
      [`(int ,e)
       (format "$~a" e)]
      [`(deref ,e ,offset)
       (format "~a(%~a)" offset e)]
      [`(,instr ,e)
       (string-append (fmt instr) (p86 e))]
      [`(,instr ,src ,dest)
       (string-append (fmt instr) (string-join `( ,(p86 src) ,(p86 dest)) ", "))]))
  (string-join
   (append
    (list
     "        .globl main"
     "main:"
     "        pushq %rbp"
     "        movq %rsp, %rbp"
     (format "        subq $~a, %rsp" (cadr prog)))
    (map p86 (cddr prog))
    (list
     (format "        addq $~a, %rsp" (cadr prog))
     "        popq %rbp"
     "        retq"))
   "\n"
   #:after-last "\n"))

(define (compile-R1 prog)
  (let ([si-out (select-instructions ((flatten-R1) ((uniquify) prog)))])
    (print-x86
     (append-print
      (patch-instructions
       (assign-homes
        si-out (make-homes si-out)))))))

(provide uniquify
         append-print
         interp-R1
         flatten-R1
         select-instructions
         assign-homes
         patch-instructions
         print-x86
         compile-R1)
