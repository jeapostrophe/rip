#lang racket/base

(require racket/list
         "model.rkt")

(struct program (fun-defns) #:mutable)
(struct results (rs) #:mutable)

(define (insert-fd! prog fd)
  (set-program-fun-defns! prog
                          (hash-set (program-fun-defns prog)
                                    (fun-defn-name fd) 
                                    fd)))



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
            (hasheq)))

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

(define FDs 
  (program (hasheq 'add f1 
                   'cube f2 
                   'pow f3)))

(define RESULTs 
  (results (list (testcase-result (testcase (list 1 2) 3)
                                  (list (fun-call 'add (list 1 2) 3)
                                        (fun-call 'add (list 1 1) 2)
                                        (fun-call 'add (list 1 0) 1))))))

(provide (all-defined-out))