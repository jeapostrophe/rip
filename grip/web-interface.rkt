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
     (body (h1 "Under construction")))))

;; xxx convert functions below to produce html

;;; print-results : (list testcase-result) -> -
;(define (print-results results)
;  (for ([result (in-list results)])
;    (cond
;      [(testcase-result? result)
;       (print-tc-result result)]
;      [(property-result? result)
;       (print-prop-result result)]
;      [(property-result/tc? result)
;       (print-prop-result/tc result)])))
;
;;; print-tc-result : testcase-result -> -
;(define (print-tc-result result)
;  (match-define (testcase-result tc trace) 
;    result)
;  (match-define (fun-call fd in out) 
;    (first trace))
;  `(div ,(string-append
;         "Test case for \""
;          (symbol->string fd)
;          "\" with input ("
;          (slist->string in)
;          "), produced "
;          (number->string out)
;          ", expected "
;          (number->string (testcase-output tc))
;          ".\n"
;          ))
;  ;(pre-print-trace (rest trace))
;  )
;
;(define (slist->string slst)
;  (string-join (map number->string slst) " "))
;
;;; print-prop-result : property-result -> -
;(define (print-prop-result result)
;  (match-define (property-result p-name trace) 
;    result)
;  (match-define (fun-call fd in out) 
;    (first trace))
;  `(div ,(string-append "The function failed property "
;          (symbol->string p-name)
;          " with input "
;          in
;          " and output "
;          out
;          ".\n"))
;  ;(pre-print-trace (rest trace))
;  )
;
;;; print-prop-result/tc : property-result/tc -> -
;(define (print-prop-result/tc result)
;  (match-define (property-result/tc p-name tc) 
;    result)
;  `(div ,(string-append 
;          "Test case ["
;          (testcase-input tc)
;          " "
;          (testcase-output tc)
;          "] fails to match property " 
;          (symbol->string p-name)
;          ".\n")))
;
;;; pre-print-trace : (list fun-call?) -> -
;#;
;(define (pre-print-trace trace) 
;  (unless (empty? trace)
;    (printf "The following is a stack trace:\n")
;    (print-trace trace)))
;
;;; print-trace : (list fun-calls?) -> -
;#;
;(define (print-trace fun-calls)
;  (cond 
;    [(empty? fun-calls)
;     (printf "\n")]
;    [else
;     (match-define (list-rest (fun-call fd input output) 
;                              rest-trace) 
;       fun-calls)
;     (printf "\"~a\" called with input: ~a and output: ~a\n"
;             fd
;             input
;             output)
;     (print-trace rest-trace)]))
;
;
;;; ensure-generator : fun-defn -> fun-defn
;#;
;(define (ensure-generator fd)
;  (cond 
;    [(empty? (fun-defn-generator fd))     
;     (printf "No generator for function parameters currently exists.\n")
;     (set-generator 
;      fd
;      (interact
;       ["Enter information about parameters"
;        (custom-generator)]
;       ["Enter an expression to specifiy parameters"
;        (expr-based-generator)]
;       ["Enter a generator function"
;        (printf "Write a function that takes zero aruguments and returns a list of parameters")
;        (read)]))]
;    [else
;     fd]))
;
;
;#;
;(define (modify-fun fun-defns fd-name)
;  (define fd (hash-ref fun-defns fd-name))
;  (interact
;   ["Edit the function"
;    (printf "Edit mode")
;    (modify-fun fun-defns fd-name)]
;   ["Test the function"
;    (print-results (test-fun fun-defns fd-name))
;    (modify-fun fun-defns fd-name)]
;   ["Add a test case"
;    (printf "Enter input for test case in a list: ")
;    (define tc-input (read))
;    (printf "Enter output for test case: ")
;    (define tc (testcase tc-input (read)))
;    (for ([bad-prop (check-new-tc fun-defns fd tc)])
;      (print-prop-result/tc (property-result/tc bad-prop tc)))
;    (modify-fun (hash-set fun-defns 
;                          fd-name
;                          (add-test-case fd tc))
;                fd-name)]
;   ["Test a property"
;    (define properties (fun-defn-properties fd))
;    (printf "Enter property name: ")
;    (define name (read))
;    (printf "Enter the number of times to run quick check: ")
;    (define count (read))
;    (define new-fds 
;      (hash-set fun-defns 
;                fd-name 
;                (ensure-generator fd)))
;    (if (hash-has-key? properties name)
;        (print-results 
;         (quick-check new-fds
;                      fd-name 
;                      name
;                      (hash-ref properties
;                                name)
;                      count))
;        (printf "No property exists with the name ~a\n" name))
;    (modify-fun new-fds fd-name)]
;   ["Add a property"
;    (printf "Enter property name: ")
;    (define name (read))
;    (printf "Enter the property function: ")
;    (define fun (read))
;    (for ([bad-tc (check-new-prop fun-defns fd fun)])
;      (print-prop-result/tc (property-result/tc name bad-tc)))
;    (modify-fun (hash-set fun-defns 
;                          fd-name
;                          (add-property fd name fun))
;                fd-name)]
;   ["Exit"
;    fun-defns]))
;#;
;(define (debugger-step fun-defns)
;  (interact
;   ["Generate worklist"
;    (print-results (gen-worklist fun-defns))
;    fun-defns]
;   ["Organize worklist"
;    (print-results (gen-worklist fun-defns))
;    fun-defns]
;   ["Modify/test a function"
;    (printf "Enter the name of the function:")
;    (define name (read))
;    (cond 
;      [(hash-has-key? fun-defns name)
;       (modify-fun fun-defns name)]
;      [else 
;       (printf "Invalid function name.")
;       fun-defns])]))