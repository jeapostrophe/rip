#lang racket/base

(require "model.rkt"
         "interp.rkt")

;; set-generator : fun-defn ( -> list?) -> fun-defn
(define (set-generator fd gen)
  (struct-copy fun-defn fd
               [generator
                gen]))

;; add-test-case : fun-defn testcase -> fun-defn
(define (add-test-case fd tc)
  (struct-copy fun-defn fd
               [test-cases
                (cons tc (fun-defn-test-cases fd))]))

;; rm-test-case : fun-defn testcase 
(define (rm-test-case fd tc)
  (struct-copy fun-defn fd
               [test-cases 
                (test-cases/expt fd tc)]))


;; test-cases/expt : fun-defn testcase -> (list testcase?) 
(define (test-cases/expt fd test-case)
  (remove test-case (fun-defn-test-cases fd)))

;; rm-property : fun-defn property -> fun-defn
(define (rm-property fd name)
  (struct-copy fun-defn fd 
               [properties 
                (hash-remove (fun-defn-properties fd)
                             name)]))

;; add-property : fun-defn name fun -> fun-defn
(define (add-property fd name fun)
  (struct-copy fun-defn fd
               [properties 
                (hash-set (fun-defn-properties fd)
                          name 
                          fun)]))

;; check-new-tc : fun-defn testcase -> (list result?)
(define (check-new-tc fun-defns fd tc)
  (for/list ([(name fun) (in-hash (fun-defn-properties fd))]
             #:unless (check-property fun-defns 
                                      fd 
                                      (testcase-input tc) 
                                      fun))
    name))

;; check-new-prop : fun-defn lambda-function -> (list result?)
(define (check-new-prop fun-defns fd pfun)
  (for/list ([tc (in-list (fun-defn-test-cases fd))]
             #:unless (check-property fun-defns 
                                      fd 
                                      (λ () (testcase-input tc)) 
                                      pfun))
    tc))

;; check-all-properties : fun-defn input -> (list property-result)
(define (check-all-properties fun-defns fd input)
  (for/list ([(p-name p-fun) (in-hash fun-defn-properties fd)]
             #:unless (result-success (check-property fun-defns fd input p-fun)))
    (property-result p-name
                     (result-trace (check-property fun-defns fd input p-fun)))))

(provide (all-defined-out))