#lang racket/base

(require redex
         rackunit)

(define-language empty-lang)



;; EXPRESSION DEFINTIONS

(define real/g
  (λ ()
    (random)))

(define (list/g . gs)
  (λ ()
    (for/list ([g (in-list gs)])
      (g))))

(define (map/g f g)
  (λ ()
    (f (g))))

(define (between/g lo hi)
  (map/g (λ (x) (+ lo (* (- hi lo) x)))
         real/g))

(define int/g 
  (λ ()
    (generate-term empty-lang integer_1 1)))

(define string/g 
  (λ ()
    (generate-term empty-lang string_a 1)))

(define symbol/g 
  (λ ()
    (string->symbol (string/g))))

(define bool/g 
  (λ ()
    (generate-term empty-lang boolean 1)))

(define (listof/g elem/g)
  (define l
    (λ ()
      (if (zero? (random 4))
          null
          (cons (elem/g)
                (l)))))
  l)



;; GENEARTOR

(define (expr-based-generator)
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
  (eval pattern))



;; TESTS

(check-pred real? (real/g))
(check-pred integer? (int/g))
(check-pred real? ((between/g 2 4)))
(check-pred (λ (x) (< 2 x 4)) ((between/g 2 4)))
(check-pred boolean? (bool/g))
(check-pred symbol? (symbol/g))
(check-pred string? (string/g))
(check-pred list? ((list/g int/g int/g)))
(check-pred list? ((listof/g int/g)))

(provide
 expr-based-generator)
