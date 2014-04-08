#lang racket/base

(require racket/list
         racket/match
         rackunit
         "model.rkt"
         "editor.rkt"
         "interp.rkt"
         "interact.rkt"
         "expr-based-qc.rkt"
         "custom-qc.rkt")


;; print-results : (list testcase-result) -> -
(define (print-results results)
  (for ([result (in-list results)])
    (cond
      [(testcase-result? result)
       (print-tc-result result)]
      [(property-result? result)
       (print-prop-result result)]
      [(property-result/tc? result)
       (print-prop-result/tc result)])))

;; print-tc-result : testcase-result -> -
(define (print-tc-result result)
  (match-define (testcase-result tc trace) 
    result)
  (match-define (fun-call fd in out) 
    (first trace))
  (printf "Test case for \"~a\" with input ~a, produced ~a, expected ~a.\n"
          fd
          in
          out
          (testcase-output tc))
  (pre-print-trace (rest trace)))

;; print-prop-result : property-result -> -
(define (print-prop-result result)
  (match-define (property-result p-name trace) 
    result)
  (match-define (fun-call fd in out) 
    (first trace))
  (printf "The function failed property ~a with input ~a and output ~a.\n"
          p-name
          in
          out)
  (pre-print-trace (rest trace)))

;; print-prop-result/tc : property-result/tc -> -
(define (print-prop-result/tc result)
  (match-define (property-result/tc p-name tc) 
    result)
  (printf "Test case [~a ~a] fails to match property ~a\n" 
          (testcase-input tc)
          (testcase-output tc)
          p-name))

;; pre-print-trace : (list fun-call?) -> -
(define (pre-print-trace trace) 
  (unless (empty? trace)
    (printf "The following is a stack trace:\n")
    (print-trace trace)))

;; print-trace : (list fun-calls?) -> -
(define (print-trace fun-calls)
  (cond 
    [(empty? fun-calls)
     (printf "\n")]
    [else
     (match-define (list-rest (fun-call fd input output) 
                              rest-trace) 
       fun-calls)
     (printf "\"~a\" called with input: ~a and output: ~a\n"
             fd
             input
             output)
     (print-trace rest-trace)]))


;; ensure-generator : fun-defn -> fun-defn
(define (ensure-generator fd)
  (cond 
    [(empty? (fun-defn-generator fd))     
     (printf "No generator for function parameters currently exists.\n")
     (set-generator 
      fd
      (interact
       ["Enter information about parameters"
        (custom-generator)]
       ["Enter an expression to specifiy parameters"
        (expr-based-generator)]
       ["Enter a generator function"
        (printf "Write a function that takes zero aruguments and returns a list of parameters")
        (read)]))]
    [else
     fd]))

(define (modify-fun fun-defns fd-name)
  (define fd (hash-ref fun-defns fd-name))
  (interact
   ["Edit the function"
    (printf "Edit mode")
    (modify-fun fun-defns fd-name)]
   ["Test the function"
    (print-results (test-fun fun-defns fd-name))
    (modify-fun fun-defns fd-name)]
   ["Add a test case"
    (printf "Enter input for test case in a list: ")
    (define tc-input (read))
    (printf "Enter output for test case: ")
    (define tc (testcase tc-input (read)))
    (for ([bad-prop (check-new-tc fun-defns fd tc)])
      (print-prop-result/tc (property-result/tc bad-prop tc)))
    (modify-fun (hash-set fun-defns 
                          fd-name
                          (add-test-case fd tc))
                fd-name)]
   ["Test a property"
    (define properties (fun-defn-properties fd))
    (printf "Enter property name: ")
    (define name (read))
    (printf "Enter the number of times to run quick check: ")
    (define count (read))
    (define new-fds 
      (hash-set fun-defns 
                fd-name 
                (ensure-generator fd)))
    (if (hash-has-key? properties name)
        (print-results 
         (quick-check new-fds
                      fd-name 
                      name
                      (hash-ref properties
                                name)
                      count))
        (printf "No property exists with the name ~a\n" name))
    (modify-fun new-fds fd-name)]
   ["Add a property"
    (printf "Enter property name: ")
    (define name (read))
    (printf "Enter the property function: ")
    (define fun (read))
    (for ([bad-tc (check-new-prop fun-defns fd fun)])
      (print-prop-result/tc (property-result/tc name bad-tc)))
    (modify-fun (hash-set fun-defns 
                          fd-name
                          (add-property fd name fun))
                fd-name)]
   ["Exit"
    fun-defns]))

(define (debugger-step fun-defns)
  (interact
   ["Generate worklist"
    (print-results (gen-worklist fun-defns))
    fun-defns]
   ["Organize worklist"
    (print-results (gen-worklist fun-defns))
    fun-defns]
   ["Modify/test a function"
    (printf "Enter the name of the function:")
    (define name (read))
    (cond 
      [(hash-has-key? fun-defns name)
       (modify-fun fun-defns name)]
      [else 
       (printf "Invalid function name.")
       fun-defns])]))



;; TESTS

(define-syntax-rule (check-debugger-step in in-str out out-str)
  (let ()
    (define outs (open-output-string))
    (check-equal? (parameterize ([current-input-port (open-input-string in-str)]
                                 [current-output-port outs])
                    (debugger-step in))
                  out)
    #;
    (check-equal? (get-output-string outs)
                  out-str)))

(check-debugger-step (hasheq) "1" (hasheq) "")

;; lambda->fun-defn : string (A -> B) -> fun-defn
(define (lambda->fun-defn name lam)
  (fun-defn name lam empty empty (hasheq)))

(define (add-test-case* fd . tcs)
  (foldr (λ (tc fd) (add-test-case fd tc))
         fd tcs))

(define (add-property* fd . properties)
  (foldr (λ (p fd) (add-property fd (car p) (cdr p)))
         fd properties))

(define f1
  (add-test-case* (lambda->fun-defn 'add '(λ (x y) (+ x y)))
                  (testcase (list 2 3) 5)
                  (testcase (list 2 -8) -6)
                  (testcase (list 0 1) 1)))

(define f2 
  (add-test-case* (lambda->fun-defn 'cube '(λ (x) (* x x x)))
                  (testcase (list 2) 8)
                  (testcase (list 3) 9)
                  (testcase (list -3) -9)
                  (testcase (list 0) 0)))

(define f3
  (fun-defn 'pow 
            '(λ (base exp) 
               (if (zero? exp)
                   1
                   (add base (pow base (- exp 1)))))
            empty
            (list (testcase (list 2 3) 8)
                  (testcase (list 3 2) 9)
                  (testcase (list -3 3) -27)
                  (testcase (list 123 0) 1)
                  (testcase (list 0 0) 1)
                  (testcase (list 0 1) 0))
            (hasheq)))

(define (debugger fun-defns)
  (debugger (debugger-step fun-defns)))

(debugger 
 (hasheq 'add f1
         'cube f2
         'pow f3))
