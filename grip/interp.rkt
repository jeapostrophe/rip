#lang racket/base

(require rackunit
         racket/match
         racket/list
         "model.rkt")

;; records : (list fun-call)
(define records empty)

;; record! : string (list value) value -> -
(define (record! f i o)
  (set! records (cons (fun-call f i o) 
                      records)))

;; Anytime a function is called with input that matches a testcase then
;; the expected output of the testcase is used. If there is not a testcase,
;; we check the output to ensure that it fulfills all of the properties
;; of the function and alert the user of all properties that are violated.

;; check-test-case : (list fun-defn) fun-defn testcase -> result
;; This function runs the specified function with the testcase input and
;; produces a stack trace of all function calls with their input/output.
(define (check-test-case fun-defns fd tc)
  (set! records empty)
  (match-define (testcase input output) tc)
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns])
    (add-assumed-fds-to-ns! (remove fd 
                                    (hash-values fun-defns)))
    (add-test-fd-to-ns! fd input)
    (result (equal? (apply (eval (fun-defn-name fd))
                           input)
                    output) 
            records)))

;; check-property : (list fun-defn) fun-defn ( -> list) ( -> bool) -> result
;; This function runs the specified property with the provided input and
;; produces a stack trace of all function calls with their input/output.
(define (check-property fun-defns fd generator-fun p-fun)
  (set! records empty)
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns])
    (add-assumed-fds-to-ns! (remove fd 
                                    (hash-values fun-defns)))
    (define input ((eval generator-fun)))
    (add-test-fd-to-ns! fd input)
    (result (apply (eval p-fun) input) 
            records)))

(define (run-fun fun-defns fun)
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns])
    (add-assumed-fds-to-ns! (hash-values fun-defns))))

;; add-assumed-fds-to-ns! (list fun-defn) -> -
(define (add-assumed-fds-to-ns! fds)
  (for ([fd (in-list fds)])
    (match-define (fun-defn name code gen tcs props) fd)
    (add-fd-to-ns! name code tcs)))

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
                  (apply (eval code) params)))
     (record! name params output)
     output)))

;; add-test-fd-to-ns! fun-defn testcase -> -
(define (add-test-fd-to-ns! fd input)
  (match-define (fun-defn name code gen tcs props) fd)
  (add-fd-to-ns! name 
                 code 
                 (remove input 
                         tcs 
                         (λ (in tc)
                           (equal? in 
                                   (testcase-input tc))))))


;; TESTS
(define f1
  (fun-defn 'add 
            '(λ (x y) (+ x (+ x y)))
            empty
            (list (testcase (list 2 3) 5)
                  (testcase (list 2 -8) -6)
                  (testcase (list 0 1) 1))
            (hasheq)))

(define f2 
  (fun-defn 'cube 
            '(λ (x) (* x x x))
            empty
            (list (testcase (list 2) 8)
                  (testcase (list 3) 9)
                  (testcase (list -3) -9)
                  (testcase (list 0) 0))
            (hasheq 'increasing 
                    (λ (x) (> (f2 x) x))
                    'super-increasing 
                    (λ (x) (> (f2 x) (* x x))))))

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

(define fun-defns (hasheq 'f1 f1
                          'f2 f2
                          'f3 f3))

(define (test-add-assumed-fds)
  (define test-ns (make-base-namespace))
  (parameterize ([current-namespace test-ns])
    (add-assumed-fds-to-ns! (hash-values fun-defns))
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
    (check-equal? (length records) 7)
    
    (check-equal? ((eval '(λ () (add 2 10)))) 14)
    (check-equal? ((eval '(λ () (add 5 5)))) 15)
    (check-equal? ((eval '(λ () (add 1 50)))) 52)
    (check-equal? (length records) 10)))

(test-add-assumed-fds)

(define (test-records) 
  (set! records empty)
  (define test-ns (make-base-namespace))
  (parameterize ([current-namespace test-ns])
    (add-assumed-fds-to-ns! (list f1 f2))
    (check-equal? ((eval '(λ () (add 2 3)))) 5)
    (check-equal? (length records) 1)))

(test-records)

(check-false (result-success (check-property (hasheq 'f1 f1 
                                                     'f2 f2) 
                                             f1 
                                             '(λ () 
                                                (list (random))) 
                                             '(λ (r) 
                                                (< r (cube r))))))
(check-false (result-success (check-test-case fun-defns 
                                              f1 
                                              (testcase (list 2 3) 5))))

;;test recursion
(check-true (result-success (check-test-case fun-defns 
                                             f3 
                                             (testcase (list 5 2) 25))))

(provide check-test-case
         check-property)
