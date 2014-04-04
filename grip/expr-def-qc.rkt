#lang racket/base

(require redex/reduction-semantics
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
 (all-defined-out))