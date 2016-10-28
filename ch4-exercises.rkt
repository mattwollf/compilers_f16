#lang racket/base

(require racket/fixnum
         racket/match
         racket/list
         racket/string
         racket/set
         racket/trace
         graph
         "utilities.rkt"
         )

(define primitives (set '+ '- 'eq? '< '<= '> '>= 'not 'read))
(define cmp-primitives (set 'eq? '< '<= '> '>=))
(define (cmp? cmp) (set-member? cmp-primitives cmp))
(define boolean->integer (match-lambda [#t 1] [#f 0] [(or (? integer? id) (? symbol? id)) id]))
(define cmp->cc (match-lambda ['eq? 'e] ['< 'l] ['<= 'le] ['> 'g] ['>= 'lg]))

(define  (interp-op op)
  (match
    op
    ['+ fx+]
    ['- (lambda (n) (fx- 0 n))]
    ['not (lambda (v) (match v [#t #f] [#f #t]))]
    ['read (read-fixnum)]
    ['eq? (lambda (v1 v2)
            (cond [(or (and (fixnum? v1) (fixnum? v2))
                       (and (boolean? v1) (boolean? v2))
                       (and (vector? v1) (vector? v2)))
                   (eq? v1 v2)]))]
    ['< (lambda (v1 v2)
          (cond [(and (fixnum? v1) (fixnum? v2))
                 (< v1 v2)]))]
    ['<= (lambda (v1 v2)
           (cond [(and (fixnum? v1) (fixnum? v2))
                  (<= v1 v2)]))]
    ['> (lambda (v1 v2)
          (cond [(and (fixnum? v1) (fixnum? v2))
                 (> v1 v2)]))]
    ['>= (lambda (v1 v2)
           (cond [(and (fixnum? v1) (fixnum? v2))
                  (>= v1 v2)]))]
    [else (error 'interp-op "unknown operator")]))

(define (interp-R2 env)
  (lambda (e)
    (define recur (interp-R2 env))
    (match
      e
      [(? symbol?) (lookup env e)]
      [(? boolean?) e]
      [(? fixnum?) e]
      [`(if ,(app recur cnd) ,thn ,els)
        (match cnd
               [#t (recur thn)]
               [#f (recur els)])]
      [`(not ,(app recur v))
        (match v [#t #f] [#f #t])]
      [`(and ,(app recur v1) ,e2)
        (match v1
               [#t (match (recur e2) [#t #t] [#f #f])]
               [#f #f])]
      [`(,op ,(app recur args) ...)
        #:when (set-member? primitives op)
        (apply (interp-op op) args)]
      )))

(define (typecheck-R2 env)
  (define (integer-cmp? x) (set-member? (set '< '<= '> '>=) x))
  (define (integer-op? x) (set-member? (set '+ '-) x))
  (define (boolean-op? x) (set-member? (set 'and 'or 'not) x))
  (lambda (e)
    (define recur (typecheck-R2 env))
    (match
      e
      [(? fixnum?) 'Integer]
      [(? boolean?) 'Boolean]
      [(? symbol?)  (lookup e env)]
      [`(let ([,x ,(app recur T)]) ,body)
        (define new-env (cons (cons x T) env))
        ((typecheck-R2 new-env) body)]
      [`(if ,cnd ,thn ,els)
        (cond [(not (eq? 'Boolean (recur cnd))) (error 'typecheck-R2 "if_expects_a_boolean_in_first_clause")]
              [(eq? (recur thn) (recur els)) (recur thn)]
              [else (error 'typecheck-R2 "if clauses type mismatch")])]
      [`(,op ,(app recur args) ...)
        #:when (integer-op? op)
        (cond [(andmap (lambda (x) (eq? 'Integer x)) args) 'Integer]
              [else (error 'typecheck-R2 "~a expects Integer operands" op)])]
      [`(,op ,(app recur args) ...)
        #:when (integer-cmp? op)
        (cond [(andmap (lambda (x) (eq? 'Integer x)) args) 'Boolean]
              [else (error 'typecheck-R2 "~a expects Integer operands" op)])]
      [`(,op ,(app recur args) ...)
        #:when (boolean-op? op)
        (cond [(andmap (lambda (x) (eq? 'Boolean x)) args) 'Boolean]
              [else (error 'typecheck-R2 "~a expects Boolean operands" op)])]
      [`(program ,body)
        (define ty ((typecheck-R2 '()) body))
        `(program (type ,ty) ,body)]
      )))


(define (uniquify [alist null])
  (λ (e)
     (match
       e
       [(? symbol?) (lookup e alist)]
       [(? integer?) e]
       [(? boolean?) e]
       [`(let ([,x ,e]) ,body)
         (define new-alist (cons (cons x (gensym x)) alist))
         `(let ([,(cdr (car new-alist)) ,((uniquify alist) e)]) ,((uniquify new-alist) body))]
       [`(program ,e)
         `(program ,((uniquify alist) e))]
       [`(,op ,es ...)
         `(,op ,@(map (uniquify alist) es))]
       )))

; alist should be a lookup table.
; a lookup table is a list of cons pairs.
(define (varlist alist)
  (define helper
    (lambda (e)
      (match
        e
        [`(assign ,var ,_) var]
        [`(if ,_ ,(app helper thn) ,(app helper els)) (append thn els)]
        [(list (app helper a) ...) a]
        [`(return ,var) var]
        )))
  (remove-duplicates (flatten (map helper alist))))

(define (flatten-R2 [vals (list null null)])
  (define cmp? (lambda (x) (set-member? (set 'eq? '> '< '<= '>=) x)))
  (define arg? (lambda (x) (or (boolean? x) (symbol? x) (integer? x))))
  (match-define (list exp alist) vals)
  (lambda (e)
    (match
      e
      [(? integer?) (list e alist)]
      [(? symbol?) (list e alist)]
      [(? boolean?) (list e alist)]
      [`(read)
        (let ([new-var (gensym 'tmp)])
          (list new-var (append alist `((assign ,new-var (read))))))]
      ;added this case because handling the extra assignment in previous case was causing contract violations.
      ;consider trying rewriting flatten.
      [(or `(let ([,x ,(? integer? e)]) ,body)
           `(let ([,x ,(? symbol? e)]) ,body)
           `(let ([,x ,(? boolean? e)]) ,body))
       ((flatten-R2 (list x (append alist `((assign ,x ,e))))) body)]
      ;in this case e is probably an s-expression requiring flattening.
      ;we can remove the last assignment and replace it with the let variable.
      [`(let ([,x ,e]) ,body)
        (match-define (list ex alist-new) ((flatten-R2 (list exp alist)) e))
        ((flatten-R2 (list
                       x
                       (append
                         (reverse (cdr (reverse alist-new)))
                         (list (match (last alist-new)
                                      [`(assign ,_ ,rhs)
                                        `(assign ,x ,rhs)]))))) body)]
      [`(program ,e)
        (match-define (list expr alist-new) ((flatten-R2) e))
        (let ([vlist (varlist alist-new)])
          (list* 'program vlist (append alist-new `((return ,expr)))))]
      [`(- ,e)
        (match-define (list ex alist-new) ((flatten-R2 (list exp alist)) e))
        (let* ([new-var (gensym 'tmp)]
               [new-exp `((assign ,new-var (- ,ex)))])
          (list new-var (append alist-new new-exp)))]
      [`(not ,e)
        (match-define (list ex alist-new) ((flatten-R2 (list exp alist)) e))
        (let* ([new-var (gensym 'tmp)]
               [new-exp `((assign ,new-var (not ,ex)))])
          (list new-var (append alist-new new-exp)))]
      [`(,(? cmp? cmp) ,e1 ,e2)
        (match-define (list lhs-exp lhs-al) ((flatten-R2 (list null alist)) e1))
        (match-define (list rhs-exp rhs-al) ((flatten-R2 (list lhs-exp lhs-al)) e2))
        (let* ([new-var (gensym 'tmp)]
               [new-exp `((assign ,new-var (,cmp ,lhs-exp ,rhs-exp)))])
          (list new-var (append rhs-al new-exp)))]
      [`(if ,cnd ,thn ,els)
        (match-define (list ex alist-new) ((flatten-R2) cnd))
        (match-define (list thn-ex thn-alist) ((flatten-R2) thn))
        (match-define (list els-ex els-alist) ((flatten-R2) els))
        (let* ([new-var (gensym 'if)]
               [new-exp `((if (eq? #t ,ex) ,(append thn-alist `((assign ,new-var ,thn-ex))) ,(append els-alist `((assign ,new-var ,els-ex)))))])
          (list new-var (append alist-new new-exp)))]

      [`(+ ,e1 ,e2)
        (match-define (list lhs-exp lhs-al) ((flatten-R2 (list null alist)) e1))
        (match-define (list rhs-exp rhs-al) ((flatten-R2 (list lhs-exp lhs-al)) e2))
        (let* ([new-var (gensym 'tmp)]
               [new-exp `((assign ,new-var (+ ,lhs-exp ,rhs-exp)))])
          (list new-var (append rhs-al new-exp)))]
      [`(and ,e1 ,e2)
        (match-define (list lhs-exp lhs-al) ((flatten-R2 (list null alist)) e1))
        (match-define (list rhs-exp rhs-al) ((flatten-R2 (list null alist)) e2))
        (let* ([new-var (gensym 'tmp)]
               [new-exp `((if (eq? #t ,lhs-exp)
                            (,(append rhs-al `(assign ,new-var (eq? #t ,rhs-exp))))
                            ((assign ,new-var #f))))])
          (list new-var (append lhs-al new-exp)))]
      )))

(define test '(program (let ([x (if #f 0 42)]) x)))

(define (select-instructions prog)
  (define arg->decl (match-lambda [(? integer?) 'int] [(? symbol?) 'var] [(? boolean?) 'int]))
  (define si
    (lambda (alist asm)
      (if 
        (null? alist) 
        asm
        (match
          (car alist)
          ; basic integer/symbol assignment
          [(or `(assign ,lhs ,(? integer? e))
               `(assign ,lhs ,(? symbol? e)))
           (si (cdr alist)
               (cons `(movq (,(arg->decl e) ,e) (var ,lhs)) asm))]
          [`(assign ,lhs ,(? boolean? e))
            (si (cdr alist)
                (cons `(movq (int ,(boolean->integer e)) (var ,lhs)) asm))]
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
          ; TODO figire out if these can be combined, only difference is var/int
          [`(assign ,lhs (+ ,(? symbol? e1) ,(? symbol? e2)))
            (si (cdr alist)
                (cons `(addq (var ,e1) (var ,lhs))
                      (cons `(movq (var ,e2) (var ,lhs)) asm)))]
          [`(assign ,lhs (read))
            (si (cdr alist)
                (cons `(movq (reg rax) (var ,lhs)) asm))]
          [`(assign ,lhs (- ,lhs))
            (si (cdr alist)
                (cons `(negq (var ,lhs)) asm))]
          [`(assign ,lhs (- ,(? integer? e)))
            (si (cdr alist)
                (cons `(negq (var ,lhs))
                      (cons `(movq (int ,e) (var ,lhs)) asm)))]
          ; TODO figure out if we can combine these. they are the same except for the var/int in the movq
          [`(assign ,lhs (- ,(? symbol? e)))
            (si (cdr alist)
                (cons `(negq (var ,lhs))
                      (cons `(movq (var ,e) (var ,lhs)) asm)))]
          [`(assign ,lhs (not ,b))
            (si (cdr alist)
                (cons `(xorq 1 ,((arg->decl b) (boolean->integer b))) asm))]
          [`(assign ,lhs (,(? cmp? cmp) ,e1 ,e2))
            (si (cdr alist)
                (cons `(movzbq (byte-reg al) (var ,lhs))
                      (cons `(set ,(cmp->cc cmp) (byte-reg al))
                            (cons `(cmpq (,(arg->decl e1) ,(boolean->integer e1)) (,(arg->decl e2) ,(boolean->integer e2))) asm))))]
          [`(if ,cnd ,thn ,els)
            (si (cdr alist)
                (cons `(if ,(reverse (si thn '())) ,(reverse (si els '()))) asm))]
          [`(return ,ret)
            (cons `(movq (var ,ret) (reg rax)) asm)]
          ))))
  (list* 'program (cadr prog) (reverse (si (cddr prog) '()))))

;; make-homes is a dumb implementation of assigning variables locations on the stack.
;;
(define (make-homes prog)
  (match-define `(program ,vars  ,instrs ...) prog)
  (define (mh offset vars ret)
    (cond [(null? vars) ret]
          [(mh (+ offset 8) (cdr vars) (cons (cons (car vars) `(deref rbp ,(- offset))) ret))]))
  (mh 8 (reverse vars) '()))

;; assigns homes to a program's variables based on the lookup list homes
;;
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
      (λ (x prev) (match x
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

(define manual-test '(program (v w x y z t.1 t.2)
                              (movq (int 1) (var v))
                              (movq (int 46) (var w))
                              (movq (var v) (var x))
                              (addq (int 7) (var x))
                              (movq (var x) (var y))
                              (addq (int 4) (var y))
                              (movq (var x) (var z))
                              (addq (var w) (var z))
                              (movq (var y) (var t.1))
                              (negq (var t.1))
                              (movq (var z) (var t.2))
                              (addq (var t.1) (var t.2))
                              (movq (var t.2) (reg rax))))

(define read-by
  (match-lambda
    [(or `(subq (var ,e1) (var ,e2))
         `(addq (var ,e1) (var ,e2)))
     (list e1 e2)]
    [(or `(addq (int ,_) (var ,e))
         `(subq (int ,_) (var ,e))
         `(movq (var ,e) ,_)
         `(negq (var ,e)))
     (list e)]
    [_ null]
    ))

(define written-by
  (match-lambda
    [(or `(negq (var ,e))
         `(,_ ,_ (var ,e)))
     (list e)]
    [_ null]
    ))

(define (uncover-live prog)
  (match-define `(program ,vars ,code ...) prog)
  (define (helper instr livelist)
    (let ([w (written-by instr)]
          [r (read-by instr)]
          [after (car livelist)])
      (let ([l-before (remove-duplicates (append (remq* w after) r))])
        (cons l-before livelist))))

  (list* 'program (list vars (cdr (foldr helper '(()) code))) code))

(define (build-interference prog)
  (match-define `(program (,vars ,live-afters) ,code ...) prog)
  (define (make-adjacencies excludes live-after)
    (foldr
      (λ (v prev)
         (cond [(list? (memq v excludes)) prev]
               [else (cons (list (car excludes) v) prev)]))
      null
      live-after))
  (define (callq-helper label)
    null)
  (define (helper live-after instr prev)
    (match instr
           [`(movq (var ,s) (var ,d))
             (append prev (make-adjacencies (list d s) live-after))]
           [`(,_ ,_ (var ,d))
             (append prev (make-adjacencies (list d) live-after))]
           [`(callq ,label)
             prev]
           [_ prev]))
  (list* 'program
         (list vars
               (undirected-graph (filter-not null? (foldl helper '() live-afters code))))
         code))

(define (color-graph g)
  (define-values (i h) (coloring/greedy g))
  h)

(define caller-save-registers '(rax rdx rcx rsi rdi r8 r9 r10 r11))
(define callee-save-registers '(rsp rbp r12 r13 r14 r15))

(define all-registers (append caller-save-registers callee-save-registers))

(define (allocate-registers registers g)
  (define-values (i h) (coloring/greedy g))
  (let ([num-registers (length registers)])
    (hash-map
      h
      (λ (key val)
         (cond [(> num-registers val) (cons key `(reg ,(list-ref registers val)))]
               [else (let ([spill (* -8 (add1 (- val num-registers)))])
                       (cons key `(deref rbp ,spill)))])))))

(define (compile-R1 prog)
  (let* ([si-out (select-instructions ((flatten-R2) ((uniquify) prog)))]
         [homes (allocate-registers caller-save-registers (cadadr (build-interference (uncover-live si-out))))])
    (print-x86
      (append-print
        (patch-instructions
          (assign-homes
            si-out homes))))))

(provide uniquify
         append-print
         flatten-R2
         select-instructions
         assign-homes
         patch-instructions
         print-x86
         typecheck-R2)
