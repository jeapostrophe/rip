#lang racket/base
(require "lang.rkt")

(define-fun add
  (λ (x y) (+ x y))
  #:tests
  [(2 3) 5]
  [(2 -8) -6]
  [(0 1) 1]
  #:properties
  [increasing
   (λ (x y) (< x (add x y)))])

(define-fun cube
  (λ (x) (* x x x))
  #:tests
  [(2) 8]
  [(3) 9]
  [(-3) -9]
  [(0) 0])

(define-fun pow
  (λ (base exp)
    (if (zero? exp)
      1
      (add base (pow base (- exp 1)))))
  #:tests
  [(2 3) 8]
  [(3 2) 9]
  [(-3 3) -27]
  [(123 0) 1]
  [(0 0) 1]
  [(0 1) 0])
