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

;; check-test-case : fun-defn testcase -> result
(define (check-test-case fd tc) (result #t empty))
;; check-property : fun-defn testcase property-function -> bool
(define (check-property-tc fd tc p-fun) #t)
;; check-test-case : fun-defn input property-function -> result
(define (check-property fun input p-fun) (result #t empty))



;; EDITOR

;; xxx add maybe-generate-input
;; fill : fun-defn hole-name expr -> fun-defn

;; fun-defn : string lambda-function (list testcase?) hasheq
(struct fun-defn (name code test-cases properties))
;; testcase : (list values) value
(struct testcase (input output) #:transparent)
;; fun-call : fun-defn (list values) value
(struct fun-call (fd input output))
;; result : bool (list fun-call?)  
(struct result (success trace))

;; lambda->fun-defn : string lambda-function -> fun-defn
(define (lambda->fun-defn name lam)
  (fun-defn name lam empty (hasheq)))

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


;; DEBUGGER

;; human's view : fun-defns -> (WORKLIST)* -> fun-defns

(define (gen-worklist fun-defns)
  ;; map : (a -> b) (list a) -> (list b)
  ;; append-map : (a -> list b) (list a) -> (list b)
  ;; (append-map test-fun (hash-values fun-defns))
  (append* 
   (for/list ([fd (in-hash-values fun-defns)])
     (test-fun fd))))

;; print-results : testcase (list fun-call?) -> -
(define (print-results results)
  (for ([result results])
    (unless (empty? result)
      (define tc (first result))
      (define trace (rest result))
      (define testcase (first trace))
      (printf "Test case with input ~a, produced ~a instead of ~a.\n"
              (fun-call-input testcase)
              (fun-call-output testcase)
              (testcase-output tc))
      (unless (empty? (rest trace))
        (printf "The following is a stack trace:\n")
        (print-trace (rest trace))))))

;; print-trace : (list fun-calls?) -> -
(define (print-trace fun-calls)
  (unless (empty? fun-calls)
    (printf "~a called with input: ~a and output: ~a\n"
            (fun-defn-name (fun-call-fd (first fun-calls)))
            (fun-call-input (first fun-calls))
            (fun-call-output (first fun-calls)))
    (print-trace (rest fun-calls))))

;; test-fun : fun-defn -> 
(define (test-fun fd)
  (for/list ([tc (fun-defn-test-cases fd)])
    (let ()
      (define r (check-test-case (fun-defn-code fd) tc))
      (if (result-success r)
          empty          
          (cons tc (result-trace r))))))

;; quick-check : fun-defn lambda-function -> xxx
(define (quick-check fd property) 
  (printf "Running quick check...\n"))


;; INTERFACE

(define-syntax-rule (interact [label . code] ...)
  (interact*
   (list (cons 'label (λ () . code)) ...)))

(define (interact* options)
  (printf "\nChoose an option\n")
  (for ([i (in-naturals 1)]
        [o (in-list options)])
    (printf " ~a. ~a\n" i (car o)))
  (define in (read))
  (if (and (number? in)
           (< 0 in (+ 1 (length options))))
      ((cdr (list-ref options (- in 1))))
      (error 'interact* "Invalid user input"))) 

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
    (let ()
      (define tc (testcase tc-input (read)))
      (for ([bad-prop (check-new-tc fd tc)])
        (printf "Your test cases fails to match property: ~a\n" bad-prop))
      (modify-fun (add-test-case fd tc)))]
   ["Test a proptery"
    (printf "Enter property name: ")
    (let ()
      (define name (read))
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
    (print-results (gen-worklist fun-defns))
    fun-defns]
   ["Organize worklist"
    (print-results (gen-worklist fun-defns))
    fun-defns]
   ["Modify/test a function"
    (printf "Enter the name of the function:")
    (let ([name (read)])
      (if (hash-has-key? fun-defns name)
          (hash-set fun-defns
                    name 
                    (modify-fun (hash-ref fun-defns name)))
          (begin (printf "Invalid function name.")
                 fun-defns)))]))

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
