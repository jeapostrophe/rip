#lang racket/base

(require rackunit
         racket/match
         racket/list
         "model.rkt")

;; Anytime a function is called with input that matches a testcase then
;; the expected output of the testcase is used. If there is not a testcase,
;; we check the output to ensure that it fulfills all of the properties
;; of the function and alert the user of all properties that are violated.

;; check-test-case : fun-defn testcase -> result
;; This function runs the specified function with the testcase input and
;; produces a stack trace of all function calls with their input/output.
(define (check-test-case fun-defns fd tc)
  (match-define (testcase input output) tc)
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns])
    (add-assumed-fds-to-ns (remove fd 
                                   (hash-values fun-defns)))
    (add-test-fd-to-ns fd input)
    (result (equal? (apply (fun-defn-code fd)
                         input)
                  output) 
          (list (fun-call fd
                          (testcase-input tc)
                          (testcase-output tc))))))

;; check-property : fun-defn input property-function -> result
;; This function runs the specified property with the provided input and
;; produces a stack trace of all function calls with their input/output.
(define (check-property fun-defns fd input p-fun)
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns])
    (add-assumed-fds-to-ns (remove fd 
                                   (hash-values fun-defns)))
    (add-test-fd-to-ns fd input)
    (result (p-fun input) 
          (list (fun-call fd
                          34
                          234)))))

;; add-assumed-fds-to-ns (list fun-defn) -> -
(define (add-assumed-fds-to-ns fds)
  (for ([fd (in-list fds)])
    (match-define (fun-defn name code gen tcs props) fd)
    (add-fd-to-ns name code tcs)))

;; add-assumed-fds-to-ns string (A -> B) (list testcase) -> -
(define (add-fd-to-ns name code tcs)
  (namespace-set-variable-value! 
   name
   (λ params
     (define matching-tcs 
       (filter (λ (tc) 
                 (equal? (testcase-input tc) params))
               tcs))
     (if (empty? matching-tcs)
         (apply code params)
         (testcase-output (first matching-tcs))))))

;; add-assumed-fds-to-ns fun-defn testcase -> -
(define (add-test-fd-to-ns fd input)
  (match-define (fun-defn name code gen tcs props) fd)
  (add-fd-to-ns name 
                code 
                (remove input 
                        tcs 
                        (λ (in tc)
                          (equal? in 
                                  (testcase-input tc))))))

;add-assumed-fun-def-to-ns : list-of-tcs name code
;add-trial-fun-def-to-ns : list-of-tcs tc name code
;
;(define (g x) (* x 7))
;(g 7) = 49
;
;===>
;
;(define (real-g x)
;  (* x 7))
;(define (fake-g x)
;  (match x
;    [7 49]
;    [_
;     (define ans (real-g x))
;     (record! g x ans)
;     ans]))
;
;....
;
;(define records empty)
;(define (record! f i o)
;  (set! records (cons (list f i o) records)))
;
;(eval tc ns) => actual-ans-tc
;(eval 'records ns)


;; TESTS
(define f1
  (fun-defn 'add 
            (λ (x y) (+ x (+ x y)))
            empty
            (list (testcase (list 2 3) 5)
                  (testcase (list 2 -8) -6)
                  (testcase (list 0 1) 1))
            (hasheq)))

(define f2 
  (fun-defn 'cube 
            (λ (x) (* x x x))
            empty
            (list (testcase (list 2) 8)
                  (testcase (list 3) 9)
                  (testcase (list -3) -9)
                  (testcase (list 0) 0))
            (hasheq 'increasing 
                    (λ (x) (> (f2 x) x))
                    'super-increasing 
                    (λ (x) (> (f2 x) (* x x))))))

(define fun-defns (hasheq 'f1 f1
                          'f2 f2))

(define (test-add-assumed-fds)
  (define test-ns (make-base-namespace))
  (parameterize ([current-namespace test-ns])
    (add-assumed-fds-to-ns (list f1 f2))
    (check-true (list? (member (fun-defn-name f1) 
                               (namespace-mapped-symbols test-ns))))
    (check-true (list? (member (fun-defn-name f2) 
                               (namespace-mapped-symbols test-ns))))
    (check-false (list? (member 'oatmeal 
                                (namespace-mapped-symbols test-ns))))
    (check-equal? ((eval '(λ () (add 2 3)))) 5)
    (check-equal? ((eval '(λ () (add 2 -8)))) -6)
    (check-equal? ((eval '(λ () (add 0 1)))) 1)
    (check-equal? ((eval '(λ () (cube 2)))) 8)
    (check-equal? ((eval '(λ () (cube 3)))) 9)
    (check-equal? ((eval '(λ () (cube -3)))) -9)
    (check-equal? ((eval '(λ () (cube 0)))) 0)
    
    (check-equal? ((eval '(λ () (add 2 10)))) 14)
    (check-equal? ((eval '(λ () (add 5 5)))) 15)
    (check-equal? ((eval '(λ () (add 1 50)))) 52)))

(test-add-assumed-fds)

(check-false (result-success (check-test-case fun-defns f1 (testcase (list 2 3) 5))))
(check-true (result-success (check-test-case fun-defns f1 (testcase (list 2 3) 7))))

(provide check-test-case
         check-property)