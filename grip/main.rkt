#lang racket/base
(require racket/list
         racket/match
         rackunit
         "model.rkt"
         "interact.rkt"
         "quick-check.rkt")

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

;; check-test-case : fun-defn testcase -> result
(define (check-test-case fd tc) 
  (result #t 
          (list (fun-call fd
                          (testcase-input tc)
                          (testcase-output tc)))))
;; check-property : fun-defn testcase property-function -> result
(define (check-property-tc fd tc p-fun) 
  (result #t 
          (list (fun-call fd
                          (testcase-input tc)
                          (testcase-output tc)))))
;; check-property : fun-defn input property-function -> result
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
             #:unless (result-success (check-property-tc fd tc fun)))
    name))

;; check-new-prop : fun-defn lambda-function -> (list result?)
(define (check-new-prop fd pfun)
  (for/list ([tc (in-list (fun-defn-test-cases fd))]
             #:unless (result-success (check-property-tc fd tc pfun)))
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
  ;; map : (a -> b) (list a) -> (list b)
  ;; append-map : (a -> list b) (list a) -> (list b)
  ;; (append-map test-fun (hash-values fun-defns))
  (append* 
   (for/list ([fd (in-hash-values fun-defns)])
     (test-fun fd))))

;; print-results-tc : (list testcase-result) -> -
(define (print-results-tc tc-results)
  (for ([tc-result (in-list tc-results)])
    (match-define (testcase-result tc trace) tc-result)
    (match-define (fun-call fd in out) (first trace))
    (printf "Test case with input ~a, produced ~a instead of ~a.\n"
            in
            out
            (testcase-output tc))
    (unless (empty? (rest trace))
      (printf "The following is a stack trace:\n")
      (print-trace (rest trace)))))

;; print-results-p : (list property-result) -> -
(define (print-results-p p-results)
  (for ([p-result p-results])
    (match-define (property-result p-name trace) p-result)
    (match-define (fun-call fd in out) (first trace))
    (printf "The function failed property ~a with input ~a and output ~a.\n"
            p-name
            in
            out)
    (unless (empty? (rest trace))
      (printf "The following is a stack trace:\n")
      (print-trace (rest trace)))))

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
  (filter (λ (item) (not (empty? item)))
          (for/list ([tc (in-list (fun-defn-test-cases fd))])
            (match-define (result success trace) (check-test-case fd tc))
            (if success
                empty          
                (testcase-result tc trace)))))

;; quick-check : fun-defn string (A -> B) integer -> 
;;                              (list property-result?)
(define (quick-check fd p-name p-fun i)   
  (printf "Running quick check...\n")
  (define (loop count results)
    (define result (quick-check-once fd
                                     p-name
                                     p-fun))
    (if (zero? count)
       results
       (if (empty? result)
           (loop (sub1 count) results)
           (loop (sub1 count) 
                 (cons result results)))]))
  (loop i empty))

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
                      (custom-generator fd)]
                     ["Enter a contract to specifiy parameter information"
                      (error 'create-generator "I haven't written this yet.")] ; xxx
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
    (print-results-tc (test-fun fd))
    (modify-fun fd)]
   ["Add a test case"
    (printf "Enter input for test case in a list: ")
    (define tc-input (read))
    (printf "Enter output for test case: ")
    (define tc (testcase tc-input (read)))
    (for ([bad-prop (check-new-tc fd tc)])
      (printf "Your test cases fails to match property: ~a\n" bad-prop))
    (modify-fun (add-test-case fd tc))]
   ["Test a property"
    (printf "Enter property name: ")
    (define name (read))
    (printf "Enter the number of times to run quick check: ")
    (define count (read))
    (define new-fd (ensure-generator fd))
    (if (hash-has-key? (fun-defn-properties new-fd) name)
        (print-results-p (quick-check new-fd 
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
      (printf "Your property conflicts with test case: ~a\n" bad-tc))
    (modify-fun (add-property fd name fun))]
   ["Exit"
    fd]))

(define (debugger-step fun-defns)
  (interact
   ["Generate worklist"
    (print-results-tc (gen-worklist fun-defns))
    fun-defns]
   ["Organize worklist"
    (print-results-tc (gen-worklist fun-defns))
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
