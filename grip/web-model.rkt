#lang racket/base
(require racket/list
         racket/runtime-path
         "file.rkt"
         "model.rkt"
         "editor.rkt")

;; lambda->fun-defn : string (A -> B) -> fun-defn
(define (lambda->fun-defn name lam)
  (fun-defn name lam empty empty (hasheq)))

(define f1
  (fun-defn 'add 
            '(λ (x y) (+ x y))
            empty
            (list (testcase (list 2 3) 5)
                  (testcase (list 2 -8) -6)
                  (testcase (list 0 1) 1))
            (hasheq 'increasing
                    '(λ (x y) (< x (add x y))))))

(define f2 
  (fun-defn 'cube 
            '(λ (x) (* x x x))
            empty
            (list (testcase (list 2) 8)
                  (testcase (list 3) 9)
                  (testcase (list -3) -9)
                  (testcase (list 0) 0))
            (hasheq)))

(define f3
  (fun-defn 'pow 
            '(λ (base exp) 
               (if (zero? exp)
                   1
                   (add base (pow base (- exp 1)))))
            empty
            (list (testcase (list 2 3) 8)
                  (testcase (list 3 2) 9)
                  (testcase (list -3 3) -27)
                  (testcase (list 123 0) 1)
                  (testcase (list 0 0) 1)
                  (testcase (list 0 1) 0))
            (hasheq)))

(define FUN-DEFNS (make-hash))
(define-runtime-path example.rkt "example.rkt")
(read-from-file! FUN-DEFNS example.rkt)

(struct output (results) #:mutable)
(define QC-RESULTS 
  (output #f))

(provide (all-defined-out))
