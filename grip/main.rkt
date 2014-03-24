#lang racket/base
(require racket/list
         rackunit)

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

;; check-test-case : fd testcase -> (success (list fun-call))
(define (check-test-case fd tc) (result #t empty))
;; check-property : fd testcase property-function -> bool
(define (check-property-tc fd tc p-fun) #t)
;; check-test-case : fd input property-function -> (success (list fun-call))
(define (check-property fun input p-fun) (result #t empty))



;; EDITOR

;; xxx add maybe-generate-input
(struct fun-defn (name code test-cases properties))
(struct testcase (input output) #:transparent)
(struct fun-call (fd input output))
(struct result (success stack-trace))
;; fill : fun-defn hole-name expr -> fun-defn

(define (lambda->fun-defn name lam)
  (fun-defn name lam empty (hasheq)))

;; add-test-case : fun-defn testcase -> fun-defn
(define (add-test-case fd tc)
  (struct-copy fun-defn fd
               [test-cases
                (cons tc (fun-defn-test-cases fd))]))

;; remove-or-revise-test-case 
(define (rm-test-case fd test-case)
  (struct-copy fun-defn fd
               [test-cases 
                (remove test-case
                        (fun-defn-test-cases fd))]))


;; all-test-cases/except : fun-defn -> (list test-case?) 
(define (test-cases/expt fd test-case)
  (remove test-case (fun-defn-test-cases fd)))

;; remove-property : fun-defn property -> fun-defn
(define (rm-property fd property)
  (struct-copy fun-defn fd 
               [properties (remove property 
                                   (fun-defn-properties fd))]))

;; add-property : fun-defn name fun -> fun-defn
(define (add-property fd name fun)
  (struct-copy fun-defn fd
               [properties 
                (hash-set (fun-defn-properties fd)
                          name 
                          fun)]))

(define (check-new-tc fd tc)
  (for/list ([(name fun) (in-hash (fun-defn-properties fd))]
             #:unless (check-property-tc fd tc fun))
    name))

(define (check-new-prop fd pfun)
  (for/list ([tc (in-list (fun-defn-test-cases fd))]
             #:unless (check-property-tc fd tc pfun))
    tc))

;; DEBUGGER

;; human's view : fun-defns -> (WORKLIST)* -> fun-defns

(define (gen-worklist fun-defns)
  ;; map : (a -> b) (list a) -> (list b)
  ;; append-map : (a -> list b) (list a) -> (list b)
  ;; (append-map test-fun (hash-values fun-defns))
  (for ([fd (in-hash-values fun-defns)])
    (test-fun fd)))

;; print-result
(define (print-results tc stack-trace)
  (let ([testcase (first stack-trace)])
    (printf "Test case with input ~a, produced ~a instead of ~a.\n"
            (fun-call-input testcase)
            (fun-call-output testcase)
            (testcase-output tc)))
  (unless (empty? (rest stack-trace))
    (printf "The following is a stack trace:\n")
    (print-stack-trace (rest stack-trace))))

(define (print-stack-trace fun-calls)
  (unless (empty? fun-calls)
    (printf "~a called with input: ~a and output: ~a\n"
            (fun-defn-name (fun-call-fd (first fun-calls)))
            (fun-call-input (first fun-calls))
            (fun-call-output (first fun-calls)))
    (print-stack-trace (rest fun-calls))))

(define (test-fun fd)
  (for ([tc (fun-defn-test-cases fd)])
    (let ([r (check-test-case (fun-defn-code fd) tc)])      
      (unless (result-success r)
        (print-results tc (result-stack-trace r))))))

(define (quick-check fd property) 
  (printf "Running quick check...\n"))

(define-syntax-rule (interact [label . code] ...)
  (interact*
   (list (cons 'label (λ () . code)) ...)))

(define (interact* options)
  (printf "\nChoose an option\n")
  (for ([i (in-naturals 1)]
        [o (in-list options)])
    (printf " ~a. ~a\n" i (car o)))
  (define in (read))
  (if (or (< in 1)
          (> in (length options)))
      (error 'interact* "Invalid user input")
      ((cdr (list-ref options (- in 1)))))) 

(define (modify-fun fd)
  (interact
   ["Edit the function"
    (printf "Edit mode")
    (modify-fun fd)]
   ["Test the function"
    (test-fun fd)
    (modify-fun fd)]
   ["Add a test case"
    (printf "Enter input for test case in a list: ")
    (define tc-input (read))
    (printf "Enter output for test case: ")
    (let ([tc (testcase tc-input (read))])
      (for ([bad-prop (check-new-tc fd tc)])
        (printf "Your test cases fails to match property: ~a\n" bad-prop))
      (modify-fun (add-test-case fd tc)))]
   ["Test a proptery"
    (printf "Enter property name: ")
    (let ([name (read)])
      (if (hash-has-key? (fun-defn-properties fd) name)
          (quick-check fd 
                       (hash-ref (fun-defn-properties fd)
                                 name))
          (printf "No property exists with the name ~a\n" name)))
    (modify-fun fd)]
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
    (gen-worklist fun-defns)
    fun-defns]
   ["Organize worklist"
    (gen-worklist fun-defns)
    fun-defns]
   ["Modify/test a function"
    (printf "Enter the name of the function:")
    (let ([name (read)])
      (if (hash-has-key? fun-defns name)
          (hash-set fun-defns
                    name 
                    (modify-fun (hash-ref fun-defns name)))
          (error 'debugger-step "Invalid function name.")))]))

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

(define f1
  (add-test-case* (lambda->fun-defn 'f1 (λ (x y) (+ x y)))
                  (testcase (list 2 3) 5)
                  (testcase (list 2 -8) -6)
                  (testcase (list 0 1) 1)))

(define f2 
  (add-test-case* (lambda->fun-defn 'f2 (λ (x) (* x x x)))
                  (testcase (list 2) 8)
                  (testcase (list 3) 9)
                  (testcase (list -3) -9)
                  (testcase (list 0) 0)))

(debugger 
 (hasheq 'f1 f1
         'f2 f2))