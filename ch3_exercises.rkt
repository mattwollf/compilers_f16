#lang racket/base

(require racket/match
         racket/list
         racket/trace
         graph
         "ch2_exercises.rkt"
         "utilities.rkt"
         )

(define raw-uncover-test '(program (let ([v 1]) (let ([w 46]) (let ([x (+ v 7)]) (let ([y (+ 4 w)]) (let ([z (+ x w)]) (+ z (- y)))))))))
(define test (select-instructions ((flatten-R1) raw-uncover-test)))

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
     (lambda (v prev)
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
  (let* ([si-out (select-instructions ((flatten-R1) ((uniquify) prog)))]
         [homes (allocate-registers caller-save-registers (cadadr (build-interference (uncover-live si-out))))])
    (print-x86
     (append-print
      (patch-instructions
       (assign-homes
        si-out homes))))))


