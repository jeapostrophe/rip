#lang racket/base
(require
 (for-syntax racket/base
             racket/list
             syntax/parse))

(define-syntax (define-fun stx)
  (syntax-parse stx
    [(_ f:id impl:expr
        (~optional (~seq #:generator gen:expr)
                   #:defaults ([gen #'()]))
        (~optional (~seq #:tests [(in:expr ...) out:expr] ...)
                   #:defaults ([(in 2) empty]
                               [(out 1) empty]))
        (~optional (~seq #:properties [pname:id pfun:expr] ...)
                   #:defaults ([(pname 1) empty]
                               [(pfun 1) empty])))
     (syntax/loc stx
       (begin
         (define f impl)
         (provide f)
         (module+ test
           (require rackunit)
           (check-equal? (f in ...) out)
           ...)
         ;; xxx compile in quickcheck?
         (module+ read
           (require "model.rkt")
           (write
            (fun-defn 'f 'impl
                      'gen
                      (list (testcase (list in ...) out)
                            ...)
                      (make-immutable-hasheq
                       (list (cons 'pname 'pfun)
                             ...)))))))]))

(provide define-fun)
