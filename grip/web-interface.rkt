#lang web-server/insta

(require racket/list
         racket/match
         rackunit
         racket/string
         "model.rkt"
         "editor.rkt"
         "interp.rkt"
         "expr-based-qc.rkt"
         "custom-qc.rkt")

(define (start request)
  (response/xexpr
   `(html
     (head (title "Web Interface"))
     (body (h1 "Under construction")
           ,(render-results (list (testcase-result (testcase (list 1 2) 3)
                                                   (list (fun-call 'add (list 1 2) 3)))))))))

;; xxx convert functions below to produce html

;; render-results : (list testcase-result) -> -
(define (render-results results)
  `(div ,@(map (λ (result) 
                 (cond
                   [(testcase-result? result)
                    (render-tc-result result)]
                   [(property-result? result)
                    (render-prop-result result)]
                   [(property-result/tc? result)
                    (render-prop-result/tc result)]
                   )) 
               results)))

;; render-tc-result : testcase-result -> xexpr
(define (render-tc-result result)
  (match-define (testcase-result tc trace) 
    result)
  (match-define (fun-call fd in out) 
    (first trace))
  `(div ,(string-append "Test case for \""
                        (symbol->string fd)
                        "\" with input "
                        (to-string in)
                        ", produced "
                        (to-string out)
                        ", expected "
                        (to-string (testcase-output tc))
                        ".\n"
                        )
        ,(render-trace (rest trace))))

;; render-prop-result : property-result -> -
(define (render-prop-result result)
  (match-define (property-result p-name trace) 
    result)
  (match-define (fun-call fd in out) 
    (first trace))
  `(div ,(string-append "The function failed property "
                        (symbol->string p-name)
                        " with input "
                        (to-string in)
                        " and output "
                        (to-string out)
                        ".\n")
        ,(render-trace (rest trace))))

;; render-prop-result/tc : property-result/tc -> -
(define (render-prop-result/tc result)
  (match-define (property-result/tc p-name tc) 
    result)
  (match-define (testcase in out) tc)
  `(div ,(string-append 
          "Test case ["
          (to-string in)
          " "
          (to-string out)
          "] fails to match property " 
          (symbol->string p-name)
          ".\n")))

(define (to-string value)
  (cond
    [(number? value)
     (number->string value)]
    [(keyword? value)
     (keyword->string value)]
    [(symbol? value)
     (symbol->string value)]
    [(path? value)
     (path->string value)]
    [(path-element? value)
     (path-element->string value)]
    [(list? value)
     (string-append "("
                    (string-join (map to-string value) " ")
                    ")")]
    [else
     "\"unable to print\""]))

;; render-trace : (list fun-call?) -> xexpr
(define (render-trace trace) 
  (if (empty? trace)
      ""
      `(div "The following is a stack trace:\n"
            (ul ,@(map render-fun-call trace)))))

;; render-fun-call : fun-call -> xexpr
(define (render-fun-call fc)
  (match-define (fun-call fd in out) fc)
  `(li ,(string-append "\""
                       (symbol->string fd)
                       "\" called with input: "
                       (to-string in)
                       " and output: "
                       (to-string out)
                       "\n")))


;; ensure-generator : fun-defn -> fun-defn
#;
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


#;
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
#;
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