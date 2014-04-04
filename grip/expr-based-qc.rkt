#lang racket/base

;; GENEARTOR

(define (expr-based-generator)
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns])
    (namespace-require '"expr-def-qc.rkt")
    (printf "Patterns are defined with the following syntax:")
    (printf "\nreal/g -> real?")
    (printf "\nint/g -> integer?")
    (printf "\n(between/g x y) -> real?")
    (printf "\nbool/g -> boolean?")
    (printf "\nstring/g -> string?")
    (printf "\nsymbol/g -> symbol?")
    (printf "\n(list/g . x) -> list?")
    (printf "\n(listof/g x) -> list?")
    (printf "\nPlease enter a pattern that produces a list of parameters: ")
    (define pattern (read))  
    (eval pattern)))

(provide
 expr-based-generator)