#lang racket/base
(require racket/list
         racket/match
         rackunit
         "model.rkt"
         "interact.rkt"
         "expr-based-qc.rkt"
         "custom-qc.rkt")

;; A Racket function is CODE

;; A Cracket function is CODE + TESTS + PROPERTIES

;; Debugger : Worklist -> Worklist repeats until WL is empty

;; Worklist = { f's first test, g's second test }
;; -> fails on g's second test
;; <- add assumption about f
;; -> checks to see if assumption is true, but isn't
;; Worklist = { f's first test, g's second test, f's second test }
;; <- use f's properties to generate 10 tests
;; -> these two fail, add them to tests or revise property?
;; <- add them

;; INTERPRETER: xxx
;; Anytime a function is called with input that matches a testcase then
;; the expected output of the testcase is used. If there is not a testcase,
;; we check the output to ensure that it fulfills all of the properties
;; of the function and alert the user of all properties that are violated.

;; check-test-case : fun-defn testcase -> result
;; This function runs the specified function with the testcase input and
;; produces a stack trace of all function calls with their input/output.
(define (check-test-case fd tc) 
  (result #f 
          (list (fun-call fd
                          (testcase-input tc)
                          (testcase-output tc)))))

;; check-property : fun-defn testcase property-function -> bool
;; This function runs the specified property with the testcase input and
;; produces a stack trace of all function calls with their input/output.
(define (check-property-tc fd tc p-fun) #f)

;; check-property : fun-defn input property-function -> result
;; This function runs the specified property with the provided input and
;; produces a stack trace of all function calls with their input/output.
(define (check-property fd input p-fun) 
  (result #f 
          (list (fun-call fd
                          input
                          41))))



;; EDITOR

;; lambda->fun-defn : string (A -> B) -> fun-defn
(define (lambda->fun-defn name lam)
  (fun-defn name lam empty empty (hasheq)))

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
(define (check-new-tc fd tc)
  (for/list ([(name fun) (in-hash (fun-defn-properties fd))]
             #:unless (check-property-tc fd tc fun))
    name))

;; check-new-prop : fun-defn lambda-function -> (list result?)
(define (check-new-prop fd pfun)
  (for/list ([tc (in-list (fun-defn-test-cases fd))]
             #:unless (check-property-tc fd tc pfun))
    tc))

;; check-all-properties : fun-defn input -> (list property-result)
(define (check-all-properties fd input)
  (for/list ([(p-name p-fun) (in-hash fun-defn-properties fd)]
             #:unless (result-success (check-property fd input p-fun)))
    (property-result p-name
                     (result-trace (check-property fd input p-fun)))))



;; DEBUGGER

;; gen-worklist : (list fun-defn) -> (list testcase-result)
(define (gen-worklist fun-defns)
  (append* 
   (for/list ([fd (in-hash-values fun-defns)])
     (test-fun fd))))

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
  (printf "Test case with input ~a, produced ~a instead of ~a.\n"
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
     (printf "~a called with input: ~a and output: ~a\n"
             (fun-defn-name fd)
             input
             output)
     (print-trace rest-trace)]))

;; test-fun : fun-defn -> testcase-result
(define (test-fun fd)
  (define testcases (fun-defn-test-cases fd))
  (filter (λ (item) 
            (not (empty? item)))
          (flatten
           (for/list ([tc (in-list testcases)])
             (match-define (result success trace) 
               (check-test-case fd tc))
             (define property-results
               (for/list ([bad-prop (check-new-tc fd tc)])
                 (property-result/tc bad-prop tc)))
             (if success
                 property-results          
                 (cons (testcase-result tc trace) 
                       property-results))))))

;; quick-check : fun-defn string (A -> B) integer -> 
;;                              (list property-result?)
(define (quick-check fd p-name p-fun count)   
  (printf "Running quick check...\n")
  (filter (λ (item) 
            (not (empty? item)))
          (for/list ([i (in-range count)])
            (quick-check-once fd
                              p-name
                              p-fun))))

;; quick-check-once : fun-defn string (A -> B) -> 
;;                              (list property-result?)
(define (quick-check-once fd p-name p-fun)
  (match-define (result success trace) 
    (check-property fd 
                    ((fun-defn-generator fd)) 
                    p-fun))
  (if success
      empty
      (property-result p-name trace)))

;; ensure-generator : fun-defn -> fun-defn
(define (ensure-generator fd)
  (cond 
    [(empty? (fun-defn-generator fd))     
     (printf "No generator for function parameters currently exists.\n")
     (set-generator fd
                    (interact
                     ["Enter information about parameters"
                      (custom-generator)]
                     ["Enter an expression to specifiy parameters"
                      (expr-based-generator)]
                     ["Enter a generator function"
                      (read)]))]
    [else
     fd]))



;; INTERFACE

(define (modify-fun fd)
  (interact
   ["Edit the function"
    (printf "Edit mode")
    (modify-fun fd)]
   ["Test the function"
    (print-results (test-fun fd))
    (modify-fun fd)]
   ["Add a test case"
    (printf "Enter input for test case in a list: ")
    (define tc-input (read))
    (printf "Enter output for test case: ")
    (define tc (testcase tc-input (read)))
    (for ([bad-prop (check-new-tc fd tc)])
      (print-prop-result/tc (property-result/tc bad-prop tc)))
    (modify-fun (add-test-case fd tc))]
   ["Test a property"
    (printf "Enter property name: ")
    (define name (read))
    (printf "Enter the number of times to run quick check: ")
    (define count (read))
    (define new-fd (ensure-generator fd))
    (if (hash-has-key? (fun-defn-properties new-fd) name)
        (print-results (quick-check new-fd 
                                    name
                                    (hash-ref (fun-defn-properties new-fd)
                                              name)
                                    count))
        (printf "No property exists with the name ~a\n" name))
    (modify-fun new-fd)]
   ["Add a property"
    (printf "Enter property name: ")
    (define name (read))
    (printf "Enter the property function: ")
    (define fun (read))
    (for ([bad-tc (check-new-prop fd fun)])
      (print-prop-result/tc (property-result/tc name bad-tc)))
    (modify-fun (add-property fd name fun))]
   ["Exit"
    fd]))

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
       (hash-set fun-defns
                 name 
                 (modify-fun (hash-ref fun-defns name)))]
      [else 
       (begin (printf "Invalid function name.")
              fun-defns)])]))



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

(define (debugger fun-defns)
  (debugger (debugger-step fun-defns)))

(define (add-test-case* fd . tcs)
  (foldr (λ (tc fd) (add-test-case fd tc))
         fd tcs))

(define (add-property* fd . properties)
  (foldr (λ (p fd) (add-property fd (car p) (cdr p)))
         fd properties))

(define f1
  (add-test-case* (lambda->fun-defn 'f1 (λ (x y) (+ x y)))
                  (testcase (list 2 3) 5)
                  (testcase (list 2 -8) -6)
                  (testcase (list 0 1) 1)))

(define f2 
  (add-property* (add-test-case* (lambda->fun-defn 'f2 (λ (x) (* x x x)))
                                 (testcase (list 2) 8)
                                 (testcase (list 3) 9)
                                 (testcase (list -3) -9)
                                 (testcase (list 0) 0))
                 (cons 'increasing 
                       (λ (x) (> (f2 x) x)))
                 (cons 'super-increasing 
                       (λ (x) (> (f2 x) (* x x))))))

(debugger 
 (hasheq 'f1 f1
         'f2 f2))
