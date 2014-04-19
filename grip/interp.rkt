#lang racket/base
(require racket/contract
         racket/match
         racket/list
         racket/function
         "model.rkt")

(module+ test
  (require rackunit)
  
  (define f1
    (fun-defn 'add 
              '(λ (x y) (+ x (+ x y)))
              empty
              (list (testcase (list 2 3) 5)
                    (testcase (list 2 -8) -6)
                    (testcase (list 0 1) 1))
              (hasheq 'increasing
                      '(λ (r) 
                         (< r (cube r))))))
  
  (define f2 
    (fun-defn 'cube 
              '(λ (x) (* x x x))
              '(λ () 
                 (list (- (* -1 (random 5039)) 1)))
              (list (testcase (list 2) 8)
                    (testcase (list 3) 9)
                    (testcase (list -3) -9)
                    (testcase (list 0) 0))
              (hasheq 'increasing 
                      '(λ (x) (> (cube x) x))
                      'super-increasing 
                      '(λ (x) (> (cube x) (* x x))))))
  
  (define f3
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
  
  (define f4
    (fun-defn 'broken 
              '(λ (f)
                 (/ f 0))
              empty
              (list (testcase (list 3) 0))
              (hasheq 'increasing
                      '(λ (r) 
                         (< r (broken r))))))
  
  (define fun-defns (hasheq 'add f1
                            'cube f2
                            'pow f3)))

;; current-records-box :  -> box
(define current-records-box (make-parameter #f))

;; record! : string (list value) value
(define (record! f i o)
  (define rb (current-records-box))
  (set-box! rb (cons (fun-call f i o) 
                     (unbox rb))))
(module+ test
  (define b (box empty))
  (parameterize ([current-records-box b])
    (record! 'f '(i) 'o))
  (check-equal? (unbox b) (list (fun-call 'f '(i) 'o)))
  
  (define rb (box empty))
  (define test-ns (make-base-namespace))
  (parameterize ([current-namespace test-ns]
                 [current-records-box rb])
    (add-assumed-fds-to-ns! (list f1 f2))
    (check-equal? ((eval '(λ () (add 2 3)))) 5)
    (check-equal? (length (unbox rb)) 1)))

;; Anytime a function is called with input that matches a testcase then
;; the expected output of the testcase is used. If there is not a testcase,
;; we check the output to ensure that it fulfills all of the properties
;; of the function and alert the user of all properties that are violated.

;; check-test-case : (list fun-defn) fun-defn testcase -> result
;; This function runs the specified function with the testcase input and
;; produces a stack trace of all function calls with their input/output.
(define (check-test-case fun-defns fd tc)
  (record-result 
   (thunk
    (match-define (testcase input output) tc)
    (add-assumed-fds-to-ns! (remove fd 
                                    (hash-values fun-defns)))
    (add-test-fd-to-ns! fd input)
    (define result (with-handlers ([exn:fail? (λ (x) x)])
                     (apply (eval (fun-defn-name fd))
                            input)))
    (if (exn:fail? result)
        #f
        (equal? result output)))))
(module+ test
  (check-false (result-success 
                (check-test-case (hash-set fun-defns
                                           'broken
                                           f4) 
                                 f4 
                                 (testcase (list 3) 0))))
  (check-false (result-success 
                (check-test-case fun-defns 
                                 f1 
                                 (testcase (list 2 3) 5))))
  (check-true (result-success 
               (check-test-case fun-defns 
                                f3 
                                (testcase (list 5 2) 25)))))

;; check-property : (list fun-defn) fun-defn ( -> list) ( -> bool) -> result
;; This function runs the specified property with the provided input and
;; produces a stack trace of all function calls with their input/output.
(define (check-property fun-defns fd generator-fun p-name)
  (record-result 
   (thunk
    (add-assumed-fds-to-ns! (remove fd 
                                    (hash-values fun-defns)))
    (define input ((eval generator-fun)))
    (define p-fun (hash-ref (fun-defn-properties fd) p-name))
    (add-test-fd-to-ns! fd input)
    (with-handlers ([exn:fail? (λ (x) #f)])
      (apply (eval p-fun) input)))))
(module+ test
  (check-false (result-success 
                (check-property fun-defns 
                                f4 
                                '(λ () 
                                   (list (random))) 
                                'increasing)))
  (for ([i (in-range 50)])
    (check-false (result-success 
                  (check-property fun-defns 
                                  f1 
                                  '(λ () 
                                     (list (random))) 
                                  'increasing)))
    (check-false (result-success 
                  (check-property fun-defns 
                                  f2 
                                  (fun-defn-generator f2) 
                                  'increasing)))
    (check-true (result-success 
                 (check-property fun-defns 
                                 f1 
                                 '(λ () 
                                    (list (+ 45 (random 45)))) 
                                 'increasing)))))

;; record-result : (A -> B) list -> result
(define (record-result fun)
  (define rb (box empty))
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns]
                 [current-records-box rb])
    (result (fun)
            (unbox rb))))

;; add-assumed-fds-to-ns! (list fun-defn) -> -
(define (add-assumed-fds-to-ns! fds)
  (for ([fd (in-list fds)])
    (match-define (fun-defn name code gen tcs props) fd)
    (add-fd-to-ns! name code tcs)))
(module+ test
  (define record-b (box empty))
  (define test-ns2 (make-base-namespace))
  (parameterize ([current-namespace test-ns2]
                 [current-records-box record-b])
    (add-assumed-fds-to-ns! (hash-values fun-defns))
    (check-true 
     (list? (member (fun-defn-name f1) 
                    (namespace-mapped-symbols test-ns2))))
    (check-true 
     (list? (member (fun-defn-name f2) 
                    (namespace-mapped-symbols test-ns2))))
    (check-false 
     (list? (member 'oatmeal 
                    (namespace-mapped-symbols test-ns2))))
    (check-equal? ((eval '(λ () (add 2 3)))) 5)
    (check-equal? ((eval '(λ () (add 2 -8)))) -6)
    (check-equal? ((eval '(λ () (add 0 1)))) 1)
    (check-equal? ((eval '(λ () (cube 2)))) 8)
    (check-equal? ((eval '(λ () (cube 3)))) 9)
    (check-equal? ((eval '(λ () (cube -3)))) -9)
    (check-equal? ((eval '(λ () (cube 0)))) 0)
    (check-equal? (length (unbox record-b)) 7)
    
    (check-equal? ((eval '(λ () (add 2 10)))) 14)
    (check-equal? ((eval '(λ () (add 5 5)))) 15)
    (check-equal? ((eval '(λ () (add 1 50)))) 52)
    (check-equal? (length (unbox record-b)) 10)))

;; add-fd-to-ns! string (A -> B) (list testcase) -> -
(define (add-fd-to-ns! name code tcs)
  (namespace-set-variable-value! 
   name
   (λ params
     (define matching-tc 
       (findf (λ (tc)
                (equal? (testcase-input tc) params))
              tcs))
     (define output (if matching-tc
                        (testcase-output matching-tc)
                        (with-handlers ([exn:fail? (λ (x) x)])
                          (apply (eval code) params))))
     (record! name params output)
     output)))
(module+ test
  (define rb2 (box empty))
  (define test-ns3 (make-base-namespace))
  (parameterize ([current-namespace test-ns3]
                 [current-records-box rb2])
    (match-define (fun-defn name code generator tcs props) f1)
    (add-fd-to-ns! name code tcs)
    (check-true 
     (list? (member 'add 
                    (namespace-mapped-symbols test-ns3))))
    (check-false 
     (list? (member (fun-defn-name f2) 
                    (namespace-mapped-symbols test-ns3))))
    (check-false 
     (list? (member 'oatmeal 
                    (namespace-mapped-symbols test-ns3))))
    (check-equal? ((eval '(λ () (add 2 3)))) 5)
    (check-equal? ((eval '(λ () (add 2 -8)))) -6)
    (check-equal? ((eval '(λ () (add 0 1)))) 1)
    (check-equal? (length (unbox rb2)) 3)
    
    (check-equal? ((eval '(λ () (add 2 10)))) 14)
    (check-equal? ((eval '(λ () (add 5 5)))) 15)
    (check-equal? ((eval '(λ () (add 1 50)))) 52)
    (check-equal? (length (unbox rb2)) 6)
    
    (match-define (fun-defn name2 code2 generator2 tcs2 props2) f4)
    (add-fd-to-ns! name2 code2 empty)
    (check-true 
     (list? (member 'broken 
                    (namespace-mapped-symbols test-ns3))))
    (check-pred exn:fail? ((eval '(λ () (broken 5)))))))

;; add-test-fd-to-ns! fun-defn testcase -> -
(define (add-test-fd-to-ns! fd input)
  (match-define (fun-defn name code gen tcs props) fd)
  (add-fd-to-ns! name 
                 code 
                 (remove input 
                         tcs 
                         (λ (in tc)
                           (equal? in 
                                   (testcase-input tc))))   ))
(module+ test
  (define rb3 (box empty))
  (define test-ns4 (make-base-namespace))
  (parameterize ([current-namespace test-ns4]
                 [current-records-box rb3])
    (add-test-fd-to-ns! (hash-ref fun-defns 'add) (list 2 -8))
    (check-true 
     (list? (member 'add 
                    (namespace-mapped-symbols test-ns4))))
    (check-false 
     (list? (member (fun-defn-name f2) 
                    (namespace-mapped-symbols test-ns4))))
    (check-false 
     (list? (member 'oatmeal 
                    (namespace-mapped-symbols test-ns4))))
    (check-equal? ((eval '(λ () (add 2 3)))) 5)
    (check-equal? ((eval '(λ () (add 2 -8)))) -4)
    (check-equal? ((eval '(λ () (add 0 1)))) 1)
    (check-equal? (length (unbox rb3)) 3)
    
    (check-equal? ((eval '(λ () (add 2 10)))) 14)
    (check-equal? ((eval '(λ () (add 5 5)))) 15)
    (check-equal? ((eval '(λ () (add 1 50)))) 52)
    (check-equal? (length (unbox rb3)) 6)))

(provide
 (contract-out
  [check-test-case (-> (hash/c symbol? fun-defn?) fun-defn? testcase? 
                       result?)]
  [check-property (-> (hash/c symbol? fun-defn?) fun-defn? any/c any/c
                      result?)]))
