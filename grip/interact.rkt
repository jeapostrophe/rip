#lang racket/base

(define-syntax-rule (interact [label . code] ...)
  (interact*
   (list (cons 'label (λ () . code)) ...)))

(define (interact* options)
  (printf "\nChoose an option\n")
  (for ([i (in-naturals 1)]
        [o (in-list options)])
    (printf " ~a. ~a\n" i (car o)))
  (define in (read))
  (cond 
    [(and (number? in)
          (< 0 in (+ 1 (length options))))
     ((cdr (list-ref options (- in 1))))]
    [else
     (printf "Invalid user input")
     (interact* options)]))

(provide 
 interact)
