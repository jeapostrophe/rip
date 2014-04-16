#lang racket/base

(require racket/list
         racket/match
         "model.rkt"
         "interp.rkt")

;; set-generator : fun-defn ( -> list?) -> fun-defn
(define (set-generator fd gen)
  (struct-copy fun-defn fd
               [generator
                gen]))

;; set-code : fun-defn (A -> B) -> fun-defn
(define (set-code fd c)
  (struct-copy fun-defn fd
               [code
                c]))

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
                                      (λ () (testcase-input tc)) 
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
             #:unless (result-success (check-property fun-defns 
                                                      fd 
                                                      (λ () input) 
                                                      p-fun)))
    (property-result p-name
                     (result-trace (check-property fun-defns 
                                                   fd 
                                                   (λ () input)
                                                   p-fun)))))


;; test-fun : (hasheq string fun-defn) string -> testcase-result
(define (test-fun fun-defns fd-name)
  (define fd 
    (hash-ref fun-defns
              fd-name))
  (define testcases 
    (fun-defn-test-cases fd))
  (flatten
   (for/list ([tc (in-list testcases)])
     (match-define (result success trace) 
       (check-test-case fun-defns fd tc))
     (define property-results
       (for/list ([bad-prop (check-new-tc fun-defns fd tc)])
         (property-result/tc bad-prop tc)))
     (if success
         property-results          
         (cons (testcase-result tc trace) 
               property-results)))))

;; quick-check : fun-defn string (A -> B) integer -> 
;;                              (list property-result?)
(define (quick-check fun-defns fd-name p-name count)   
  (filter (λ (item) 
            (not (empty? item)))
          (for/list ([i (in-range count)])
            (quick-check-once fun-defns
                              fd-name
                              p-name))))

;; quick-check-once : fun-defn string (A -> B) -> 
;;                              (list property-result?)
(define (quick-check-once fun-defns fd-name p-name)
  (define fd (hash-ref fun-defns fd-name))
  (define res
    (with-handlers ([exn:fail? (λ (x) #f)])
      (check-property fun-defns 
                      fd 
                      (fun-defn-generator fd) 
                      (hash-ref (fun-defn-properties fd)
                                p-name))))
  (match res
    [#f
     empty]
    [(result success trace)     
     (if success
         empty
         (property-result p-name trace))]))

;; gen-worklist : (list fun-defn) -> (list testcase-result)
(define (gen-worklist fun-defns)
  (append* 
   (for/list ([fd (in-hash-values fun-defns)])
     (test-fun fun-defns (fun-defn-name fd)))))


;; TESTS
(define fun1
  (fun-defn 'add 
            '(λ (x y) (+ x (+ x y)))
            empty
            (list (testcase (list 2 3) 5)
                  (testcase (list 2 -8) -6)
                  (testcase (list 0 1) 1))
            (hasheq)))

(define fun2 
  (fun-defn 'cube 
            '(λ (x) (* x x x))
            empty
            (list (testcase (list 2) 8)
                  (testcase (list 3) 9)
                  (testcase (list -3) -9)
                  (testcase (list 0) 0))
            (hasheq 'increasing 
                    '(λ (x) (> (cube x) x))
                    'super-increasing 
                    '(λ (x) (> (cube x) (* x x))))))

(define fun3
  (fun-defn 'pow 
            '(λ (base exp) 
               (if (zero? exp)
                   1
                   (* base (pow base (- exp 1)))))
            empty
            (list (testcase (list 2 3) 8)
                  (testcase (list 3 2) 9)
                  (testcase (list -3 3) -27)
                  (testcase (list 123 0) 1)
                  (testcase (list 0 0) 1)
                  (testcase (list 0 1) 0))
            (hasheq)))

(define fun-defns (hasheq 'add fun1
                          'cube fun2
                          'pow fun3))

(require rackunit)
(check-equal? (quick-check-once fun-defns 'cube 'increasing)
              empty)

(provide (all-defined-out))