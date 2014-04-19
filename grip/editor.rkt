#lang racket/base

(require racket/list
         racket/contract
         racket/match
         "model.rkt"
         "interp.rkt")

(module+ test
  (require rackunit)
  (define fun-correct
    (fun-defn 'correct
              '(λ (sym) (symbol->string sym))
              '(λ () (list (gensym)))
              (list (testcase (list 'dog) "dog")
                    (testcase (list 'pIraTe) "pIraTe"))
              (hasheq 'non-empty
                      '(λ (sym) 
                         (< 0 (string-length (correct sym)))))))
  (define fun1
    (fun-defn 'add 
              '(λ (x y) (+ x (+ x y)))
              empty
              (list (testcase (list 2 3) 5)
                    (testcase (list 2 -8) -6)
                    (testcase (list 0 1) 1))
              (hasheq 'increasing 
                      '(λ (a x) (>= (add a x) x)))))
  
  (define fun2 
    (fun-defn 'cube 
              '(λ (x) (* x x x))
              '(λ () 
                 (list (- (* -1 (random 5039)) 1)))
              (list (testcase (list 2) 8)
                    (testcase (list 3) 27)
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
                            'pow fun3
                            'correct fun-correct)))

;; set-generator : fun-defn ( -> list?) -> fun-defn
(define (set-generator fd gen)
  (struct-copy fun-defn fd
               [generator
                gen]))
(module+ test
  (define gen (λ () (random)))
  (check-equal? gen (fun-defn-generator (set-generator fun2 gen))))

;; set-code : fun-defn (A -> B) -> fun-defn
(define (set-code fd c)
  (struct-copy fun-defn fd
               [code
                c]))
(module+ test
  (define code (λ (x) (random x)))
  (check-equal? code (fun-defn-code (set-code fun2 code))))

;; add-test-case : fun-defn testcase -> fun-defn
(define (add-test-case fd tc)
  (struct-copy fun-defn fd
               [test-cases
                (cons tc (fun-defn-test-cases fd))]))
(module+ test
  (define non-empty-list? 
    (λ (x) (and (list? x)
                (not (empty? x)))))
  (define tc (testcase (list 3 4) 7))
  (check-false (member tc (fun-defn-test-cases fun1)))
  (check-pred non-empty-list?
              (member tc (fun-defn-test-cases (add-test-case fun1 tc)))))

;; rm-test-case : fun-defn testcase 
(define (rm-test-case fd tc)
  (struct-copy fun-defn fd
               [test-cases 
                (test-cases/expt fd tc)]))
(module+ test
  (define tc2 (testcase (list 2 3) 5))
  (check-pred non-empty-list?
              (member tc2 (fun-defn-test-cases fun1)))
  (check-false (member tc2 (fun-defn-test-cases (rm-test-case fun1 tc2)))))

;; test-cases/expt : fun-defn testcase -> (list testcase?) 
(define (test-cases/expt fd tc)
  (remove tc (fun-defn-test-cases fd)))
(module+ test
  (check-pred non-empty-list?
              (member tc2 (fun-defn-test-cases fun1)))
  (check-false (member tc2 (test-cases/expt fun1 tc2))))

;; rm-property : fun-defn property -> fun-defn
(define (rm-property fd name)
  (struct-copy fun-defn fd 
               [properties 
                (hash-remove (fun-defn-properties fd)
                             name)]))
(module+ test  
  (check-true (hash-has-key? (fun-defn-properties fun2)
                             'increasing))
  (check-false (hash-has-key? (fun-defn-properties 
                               (rm-property fun2 'increasing))
                              'increasing)))

;; add-property : fun-defn name fun -> fun-defn
(define (add-property fd name fun)
  (struct-copy fun-defn fd
               [properties 
                (hash-set (fun-defn-properties fd)
                          name 
                          fun)]))
(module+ test
  (check-false (hash-has-key? (fun-defn-properties fun2)
                              'decreasing))
  (check-true (hash-has-key? (fun-defn-properties 
                              (add-property fun2 'decreasing '(λ () #f)))
                             'decreasing)))

;; check-all-properties : fun-defn input -> (list property-result)
(define (check-all-properties fun-defns fd input)
  (for/list ([(p-name p-fun) (in-hash (fun-defn-properties fd))]
             #:unless (result-success (check-property fun-defns 
                                                      fd 
                                                      (λ () input) 
                                                      p-name)))
    (property-result p-name
                     (result-trace (check-property fun-defns 
                                                   fd 
                                                   (λ () input)
                                                   p-name)))))
(module+ test
  (check-pred empty?  
              (check-all-properties fun-defns fun1 (list 9 0)))
  (check-pred empty?  
              (check-all-properties fun-defns fun2 (list 2)))
  (check-pred non-empty-list?
              (check-all-properties fun-defns fun2 (list -2)))
  (check-equal? (property-result-property-name
                 (first (check-all-properties fun-defns fun2 (list -16))))
                'increasing)
  (check-equal? (property-result-property-name
                 (second (check-all-properties fun-defns fun2 (list -16))))
                'super-increasing))

;; test-fun : (hasheq string fun-defn) symbol -> (list result)
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
       (check-all-properties fun-defns fd (testcase-input tc)))
     (if success
         property-results          
         (cons (testcase-result tc trace) 
               property-results)))))
(module+ test
  (check-pred empty?  
              (test-fun fun-defns 'correct))
  (check-pred (λ (l) (and (= 2 (length l))
                          (testcase-result? (first l))
                          (testcase-result? (second l))))  
              (test-fun fun-defns 'add))
  (check-pred (λ (l) (and (= 5 (length l))
                          (testcase-result? (first l))
                          (= 4 (length (filter property-result? (rest l))))))  
              (test-fun fun-defns 'cube)))

;; quick-check : fun-defn symbol symbol integer -> 
;;                              (list property-result?)
(define (quick-check fun-defns fd-name p-name count)  
  (define fd (hash-ref fun-defns fd-name)) 
  (filter (λ (item) 
            (not (empty? item)))
          (for/list ([i (in-range count)])
            (quick-check-once fun-defns
                              fd
                              p-name))))
(module+ test
  (check-pred empty?  
              (test-fun fun-defns 'correct))
  (check-pred (λ (l) (and (= 2 (length l))
                          (testcase-result? (first l))
                          (testcase-result? (second l))))  
              (test-fun fun-defns 'add))
  (check-pred (λ (l) (and (= 5 (length l))
                          (testcase-result? (first l))
                          (= 4 (length (filter property-result? (rest l))))))  
              (test-fun fun-defns 'cube)))

;; quick-check-once : fun-defns fun-defn symbol -> property-result?
(define (quick-check-once fun-defns fd p-name)
  (define res
    (with-handlers ([exn:fail? (λ (x) #f)])
      (check-property fun-defns 
                      fd 
                      (fun-defn-generator fd)
                      p-name)))
  (match res
    [#f
     empty]
    [(result success trace)     
     (if success
         empty
         (property-result p-name trace))]))
(module+ test
  (check-equal? (quick-check-once fun-defns 'add 'increasing)
                empty)
  (check-equal? (quick-check-once fun-defns 'correct 'non-empty)
                empty)
  (check-pred property-result?
              (quick-check-once fun-defns 'cube 'increasing)))

;; gen-worklist : (list fun-defn) -> (list result)
(define (gen-worklist fun-defns)
  (append* 
   (for/list ([fd (in-hash-values fun-defns)])
     (test-fun fun-defns (fun-defn-name fd)))))

(provide
 (contract-out
  [gen-worklist (-> (hash/c symbol? fun-defn?) 
                    (listof (or/c testcase-result?
                                  property-result?)))]
  [quick-check (-> (hash/c symbol? fun-defn?) symbol? symbol? number? 
                   (listof property-result?))]
  [add-test-case (-> fun-defn? testcase? fun-defn?)]
  [set-generator (-> fun-defn? any/c fun-defn?)]
  [set-code (-> fun-defn? any/c fun-defn?)]
  [add-property (-> fun-defn? symbol? any/c fun-defn?)]
  [rm-property (-> fun-defn? symbol? fun-defn?)]
  [rm-test-case (-> fun-defn? testcase? fun-defn?)]
  [test-fun (-> (hash/c symbol? fun-defn?) symbol? 
                (listof (or/c testcase-result?
                              property-result?)))]))