#lang racket/base

(require web-server/http
         web-server/http/bindings
         web-server/dispatch
         web-server/formlets
         web-server/servlet-env
         racket/list
         racket/match
         racket/runtime-path
         racket/port
         "model.rkt"
         "editor.rkt"
         "expr-based-qc.rkt"
         "web-model.rkt")
(module+ test
  (require rackunit))

(define-runtime-path htdocs "htdocs")

;; xxx To Do List
;; - catch syntax errors


;; FRAMEWORK
(define-container
  grip-container
  (grip-dispatch grip-url))

(dispatch-rules!
 grip-container
 [("grip") grip]
 [() grip]
 [("") grip])

(define-syntax-rule
  (define-action (embed-id args ...)
    #:url url-pattern
    display-fun
    link-expr)
  (begin
    (dispatch-rules! grip-container [url-pattern action-id])
    (define (embed-id args ...)
      (display-fun (grip-url action-id args ...)))
    (define (action-id req args ...)
      (link-expr req)
      (redirect-to (grip-url grip)))))

(define-syntax-rule
  (define-formlet-action (embed-id args ...)
    title
    #:url url-pattern
    formlet-expr)
  (begin
    (define (formlet-id args ...)
      formlet-expr)
    (define-action (embed-id args ...)
      #:url url-pattern
      (λ (some-url)
        `(div ((class "well well-sm"))
              (form ((action ,some-url))
                    (legend ,title)
                    ,@(formlet-display (formlet-id args ...))
                    (div ((class "form-group"))
                         (button ((class "btn btn-default")
                                  (type "submit"))
                                 "Save")))))
      (λ (req)
        (formlet-process (formlet-id args ...) req)))))

(define-syntax-rule
  (define-modal-action (embed-id args ...)
    title
    #:url url-pattern
    formlet-expr)
  (begin
    (define (formlet-id args ...)
      formlet-expr)
    (define-action (embed-id args ...)
      #:url url-pattern
      (λ (some-url)
        `(div ((class "modal")
               (id "modal-dialog"))
              (div ((class "modal-dialog"))
                   (div ((class "modal-content"))
                        (form ((action ,some-url))
                              (div ((class "modal-header"))
                                   (button ((type "button")
                                            (class "close")
                                            (data-dismiss "modal")
                                            (aria-hidden "true"))
                                           "×")
                                   (h4 ((class "modal-title"))
                                       title))
                              (div ((class "modal-body"))
                                   ,@(formlet-display (formlet-id args ...)))
                              (div ((class "modal-footer"))
                                   (button ((type "button")
                                            (data-dismiss "modal")
                                            (class "btn btn-default"))
                                           "Cancel")
                                   (button ((type "sumbit")
                                            (class "btn btn-primary"))
                                           "Save")))))))
      (λ (req)
        (formlet-process (formlet-id args ...) req)))))

(define-syntax-rule
  (define-link-action (embed-id args ...)
    #:url url-pattern
    link-text-expr
    link-expr)
  (define-action (embed-id args ...)
    #:url url-pattern
    (λ (some-url)
      `(tr
        link-text-expr
        (td (a ((class "close")
                (href ,some-url)) "×"))))
    (λ (req) link-expr)))


;; HANDLERS

(define (grip req)
  (response/xexpr
   `(html
     (head (title "GRIP")
           (link ((rel "stylesheet")
                  (href "/bootstrap.min.css")
                  (type "text/css")))
           (script ((src "//code.jquery.com/jquery-latest.min.js")
                    (type "text/javascript")))
           (script ((src "/bootstrap.min.js")
                    (type "text/javascript")))
           (script ((src "/bootswatch.js")
                    (type "text/javascript"))))
     (body
      (div ((class "container"))
           (div ((class "page-header"))
                (div ((class "row"))
                     (h1 "Guided Racket Interactive Programming")
                     (p ((class "lead"))
                        "A test driven approach to programming")))
           (div ((class "row"))
                ,(render-errors)
                ,(qc-results)
                ,(render-fd-panel)
                ,(render-results-panel)))))))

(define-modal-action (add-qc-results fd-name)
  "QuickCheck Results"
  #:url ("grip" "fun" (string-arg) "qc-results")
  (formlet
   (#%#
    (p "Click save if you would like to save all of these as test cases.")
    (table ((class "table"))
           (thead
            (tr
             (th "Function")
             (th "Violated Property")
             (th "Input")
             (th "Output")))
           (tbody
            ,@(map (λ (result)
                     (match-define (property-result p-name trace)
                       result)
                     (match-define (fun-call fd in out)
                       (first trace))
                     `(tr ((class "danger"))
                          (td ,(to-str fd))
                          (td ,(to-str p-name))
                          (td ,(to-str in))
                          (td ,(to-str out))))
                   (get-quick-check-results)))))
   (set-fun-defn!
    (string->symbol fd-name)
    (foldr (λ (result fd)
             (match-define (fun-call fd-call-name in out)
               (first (property-result-trace result)))
             (add-test-case fd (testcase in out)))
           (get-fun-defn
            (string->symbol fd-name))
           (get-quick-check-results)))))

(define-modal-action (get-generator fd-name p-name qc-count)
  "Enter a generator function"
  #:url ("grip" "fun" (string-arg)
                "prop" (string-arg)
                "qc-count" (string-arg))
  (formlet
   (#%#
    (p ,(string-append "No generator function exists for "
                       fd-name
                       "."))
    ,{(radio-group
       (list "1. Enter an expression to specifiy parameters"
             "2. Enter a function to generate parameters")
       #:display (λ (text)`(span
                            (label ((class "control-label"))
                                   ,text)
                            (br)))) . => . rg}
    (br)
    (div ((class "form-group"))
         (label ((class "col-lg-2 control-label"))
                "Input:")
         ,{input-string . => . gen-func})
    (p (small
        "1. Expressions are defined with the following syntax:" (br)
        "real/g -> real?" (br)
        "int/g -> integer?" (br)
        "(between/g x y) -> real?" (br)
        "bool/g -> boolean?" (br)
        "string/g -> string?" (br)
        "symbol/g -> symbol?" (br)
        "(list/g . x) -> list?" (br)
        "(listof/g x) -> list?" (br)
        "Enter an expression that produces a list of parameters."))
    (p (small
        "2. Enter a function that takes zero arguments and"
        " returns a list of randomly generated parameters for "
        "your function.")))
   (begin
     (define fd-name-sym (string->symbol fd-name))
     (define fun
       (if (char=? (string-ref rg 0) #\1)
           (expr-based-generator (to-racket gen-func))
           (to-racket gen-func)))
     (set-fun-defn!
      fd-name-sym
      (set-generator (get-fun-defn
                      fd-name-sym)
                     fun))
     (run-quick-check fd-name-sym
                      (string->symbol p-name)
                      (string->number qc-count)))))

(define-formlet-action (add-new-fd)
  "Add a new function"
  #:url ("grip" "fun" "new")
  (formlet
   (div ((class "form-group"))
        (label ((class "col-lg-2 control-label"))
               "Name")
        ,{input-string . => . name})
   (if (has-fun-defn? (string->symbol name))
       (add-error! (string-append "A function already exists with the name"
                                  name))
       (set-fun-defn!
        (string->symbol name)
        (fun-defn (string->symbol name)
                  '(λ () )
                  empty
                  empty
                  (hasheq))))))

(define-formlet-action (add-new-tc name)
  "Add a new test case"
  #:url ("grip" "fun" (string-arg) "new-testcase")
  (formlet
   (#%#
    (div ((class "form-group"))
         (label ((class "col-lg-2 control-label"))
                "Input")
         ,{input-string . => . input})
    (div ((class "form-group"))
         (label ((class "col-lg-2 control-label"))
                "Output")
         ,{input-string . => . output}))
   (begin
     (define tc (check-tc-input input output))
     (when tc
       (set-fun-defn!
        (string->symbol name)
        (add-test-case (get-fun-defn
                        (string->symbol name))
                       tc))))))

(define (check-tc-input input output)
  (with-handlers ([exn:fail? (λ (e) 
                               (add-error! 
                                (string-append "The following error occurred: "
                                               e))
                               #f)])
    (define in (to-racket input))
    (cond 
      [(list? in)
       (testcase in (to-racket output))]
      [else
       (add-error! "Input for a testcase must be a list of parameters.")
       #f])))

(define-formlet-action (add-new-prop name)
  "Add a new property"
  #:url ("grip" "fun" (string-arg) "new-property")
  (formlet
   (#%#
    (div ((class "form-group"))
         (label ((class "col-lg-2 control-label"))
                "Name")
         ,{input-string . => . p-name})
    (div ((class "form-group"))
         (label ((class "col-lg-2 control-label"))
                "Function")
         ,{input-string . => . p-fun}))
   (begin 
     (when (check-property-input name p-name p-fun)
       (set-fun-defn!
        (string->symbol name)
        (add-property (get-fun-defn
                       (string->symbol name))
                      (string->symbol p-name)
                      (to-racket p-fun)))))))
(define (check-property-input fd-name p-name p-fun)
  (with-handlers ([exn:fail? (λ (e) 
                               (add-error! 
                                (string-append "The following error occurred: "
                                               e))
                               #f)])
    (cond 
      [(has-prop? (string->symbol fd-name)
                  (string->symbol p-name))
       (add-error! (string-append "A property in function definition "
                                  fd-name
                                  " already exists with the name "
                                  p-name))
       #f]
      [else
       #t])))
(module+ test
  (check-false (check-property-input "add" "increasing" "(λ (x) 123)"))
  (check-true (check-property-input "cube" "fail" "(λ (y) #f)")))

(define-link-action (remove-prop fd-name p-name code)
  #:url ("grip" "fun" (string-arg) "p-name" (string-arg)
                "p-fun" (string-arg))
  (div
   (td ,p-name)
   (td ,code))
  (set-fun-defn!
   (string->symbol fd-name)
   (rm-property (get-fun-defn
                 (string->symbol fd-name))
                (string->symbol p-name))))

(define-formlet-action (edit-code name code)
  "Edit the code below"
  #:url ("grip" "fun" (string-arg) "code" (string-arg))
  (formlet
   (div ((class "form-group"))
        ,{(to-string
           (required
            (text-input #:value
                        (string->bytes/utf-8 code)))) . => . new-code})
   (if (procedure? (to-racket new-code))
       (set-fun-defn!
        (string->symbol name)
        (set-code (get-fun-defn
                   (string->symbol name))
                  (to-racket new-code)))
       (add-error! (string-append "Code must be a procedure; given: "
                                  new-code)))))

(define-formlet-action (test-prop)
  "Run QuickCheck"
  #:url ("grip" "fun" "test-prop")
  (formlet
   (#%#
    (div ((class "form-group"))
         (label ((class "col-lg-2 control-label"))
                "Function Name ")
         ,{input-string . => . fd-name})
    (div ((class "form-group"))
         (label ((class "col-lg-2 control-label"))
                "Property Name ")
         ,{input-string . => . p-name})
    (div ((class "form-group"))
         (label ((class "col-lg-2 control-label"))
                "Number of times to test ")
         ,{input-string . => . qc-count}))
   (when (check-qc-input fd-name p-name qc-count)
     (run-quick-check (string->symbol fd-name)
                      (string->symbol p-name)
                      (string->number qc-count)))))
(define (check-qc-input fd-name p-name count)
  (with-handlers ([exn:fail? (λ (e) 
                               (add-error! 
                                (string-append "The following error occurred: "
                                               (exn-message e)))
                               #f)])
    (define pass (box #t))
    (unless (has-fun-defn? (string->symbol fd-name))
      (add-error! (string-append "No function exists with the name "
                                 fd-name))
      (set-box! pass #f))
    (unless (has-prop? (string->symbol fd-name)
                       (string->symbol p-name))
      (add-error! (string-append "No property in function definition "
                                 fd-name
                                 " exists with the name "
                                 p-name))
      (set-box! pass #f))
    (unless (positive? (to-racket count))
      (add-error! (string-append "Required a positive integer; given "
                                 count))
      (set-box! pass #f))
    (unbox pass)))
(module+ test
  (check-false (check-qc-input "cube-it" "increasing" "123"))
  (check-true (check-qc-input "add" "increasing" "123"))
  (check-false (check-qc-input "cube" "increasing" "123"))
  (check-false (check-qc-input "cube" "increasing" "nh7")))


;; FUNCTION DEFINITIONS

;; render-errors : -> xexpr
(define (render-errors)
  (define errors (get-errors))
  (clear-errors)
  (if (empty? errors)
      `(div)
      `(div ((class "alert alert-dimissable alert-danger"))
            (button ((type "button")
                     (class "close")
                     (data-dismiss "alert"))
                    "x")
            ,@(map (λ (error-msg)
                     `(p ,error-msg))
                   errors))))

;; qc-results : -> xexpr
(define (qc-results)
  (match (get-quick-check-results)
    [#f
     `(div)]
    [(vector f p k)
     `(div ,(get-generator f p (to-str k))
           (script ((type "text/javascript")
                    (src "/show-modal.js"))))]
    [(list)
     `(div ((class "alert alert-dimissable alert-success"))
           (button ((type "button")
                    (class "close")
                    (data-dismiss "alert"))
                   "x")
           "All quick check tests passed!")]
    [results
     (define fd-name
       (fun-call-fun-name (first (property-result-trace (first results)))))
     `(div ,(add-qc-results fd-name)
           (script ((type "text/javascript")
                    (src "/show-modal.js"))))]))

;; render-fd-panel : -> xexpr
(define (render-fd-panel)
  `(div ((class "panel panel-primary"))
        (div ((class "panel-heading"))
             (h3 ((class "panel-title"))
                 "Program"))
        (div ((class "panel-body"))
             (ul ((class "nav nav-tabs")
                  (style "margin-bottom: 15px;"))
                 ,@(map render-fd-tab
                        (get-fun-defns))
                 (li (a ((href "#new-fd")
                         (data-toggle "tab"))
                        "+ Function")))
             (div ((class "tab-content"))
                  ,@(map render-fd-content
                         (get-fun-defns))
                  ,(render-new-fd-form)))))

;; render-new-fd-form : -> xexpr
(define (render-new-fd-form)
  `(div ((class "tab-pane fade")
         (id "new-fd"))
        ,(add-new-fd)))

;; render-fd-tab : fun-defn -> xexpr
(define (render-fd-tab fd)
  (match-define (fun-defn name code generator tcs props) fd)
  (define tab-id (string-append "#"
                                (to-str name)))
  `(li (a ((href ,tab-id)
           (data-toggle "tab"))
          ,(to-str name))))


;; render-fd-content : fun-defn -> xexpr
(define (render-fd-content fd)
  (match-define (fun-defn name code generator tcs props) fd)
  (define fd-name (to-str name))
  (define tab-id (string-append "#" fd-name))
  `(div ((class "tab-pane fade")
         (id ,fd-name))
        (ul ((class "nav nav-tabs")
             (style "margin-bottom: 15px;"))
            (li (a ((href ,(string-append tab-id "-code"))
                    (data-toggle "tab"))
                   "Code"))
            (li (a ((href ,(string-append tab-id "-testcases"))
                    (data-toggle "tab"))
                   "Test Cases"))
            (li (a ((href ,(string-append tab-id "-properties"))
                    (data-toggle "tab"))
                   "Properties")))
        (div ((class "tab-content"))
             (div ((class "tab-pane fade")
                   (id ,(string-append fd-name "-code")))
                  ,(edit-code fd-name (to-str code)))
             (div ((class "tab-pane fade")
                   (id ,(string-append fd-name "-testcases")))
                  ,(render-testcases tcs fd-name))
             (div ((class "tab-pane fade")
                   (id ,(string-append fd-name "-properties")))
                  ,(render-properties props fd-name)))))

;; render-testcases : (list testcase) string -> xexpr
(define (render-testcases tcs fd-name)
  `(div
    ,(if (empty? tcs)
         `(div ((class "alert alert-dimissable alert-info"))
               (button ((type "button")
                        (class "close")
                        (data-dismiss "alert"))
                       "x")
               "Currently, there are no test cases for this function.")
         `(table ((class "table"))
                 (thead
                  (tr (th "Input")
                      (th "Output")))
                 (tbody ,@(map render-testcase tcs))))
    ,(add-new-tc fd-name)))

;; render-testcase : testcase
(define (render-testcase tc)
  (match-define (testcase in out) tc)
  `(tr (td ,(to-str in)) (td ,(to-str out))))

;; render-properties : hasheq string -> xexpr
(define (render-properties props fd-name)
  `(div
    ,(if (zero? (hash-count props))
         `(div ((class "alert alert-dimissable alert-info"))
               (button ((type "button")
                        (class "close")
                        (data-dismiss "alert"))
                       "x")
               "Currently, there are no properties for this function.")
         `(table ((class "table"))
                 (thead
                  (tr (th "Name")
                      (th "Function")
                      (th)))
                 (tbody ,@(hash-map props
                                    (λ (name fun)
                                      (remove-prop fd-name
                                                   (to-str name)
                                                   (to-str fun)))))))
    ,(add-new-prop fd-name)))



;; RESULTS

;; render-results-panel : -> xexpr
(define (render-results-panel)
  `(div ((class "panel panel-info"))
        (div ((class "panel-heading"))
             (h3 ((class "panel-title"))"Results"))
        (div ((class "panel-body"))
             ,(render-results (get-worklist))
             ,(test-prop))))

;; render-results : -> xexpr
(define (render-results results)
  (if (empty? results)
      `(div ((class "alert alert-dismissable alert-success"))
            (button ((type "button")
                     (class "close")
                     (data-dismiss "alert"))
                    "x")
            "All checks passed successfully!")
      `(table ((class "table table-striped"))
              (thead (tr
                      (th "Function")
                      (th "Input")
                      (th "Actual Output")
                      (th "Expected Output")))
              ,@(map (λ (result)
                       (cond
                         [(testcase-result? result)
                          (render-tc-result result)]
                         [(property-result? result)
                          (render-prop-result result)]
                         [(property-result/tc? result)
                          (render-prop-result/tc result)]
                         ))
                     results))))

;; render-tc-result : testcase-result -> xexpr
(define (render-tc-result result)
  (match-define (testcase-result tc trace)
    result)
  (match-define (fun-call fd in out)
    (first trace))
  `(tbody (tr ((class "danger"))
              (td ,(to-str fd))
              (td ,(to-str in))
              (td ,(to-str out))
              (td ,(to-str (testcase-output tc))))
          ,@(map render-fun-call (rest trace))))

;; render-prop-result : property-result -> xexpr
(define (render-prop-result result)
  (match-define (property-result p-name trace)
    result)
  (match-define (fun-call fd in out)
    (first trace))
  `(tbody (tr ((class "danger"))
              (td ,(string-append (to-str fd) ": "
                                  (to-str p-name)))
              (td ,(to-str in))
              (td ,(to-str out))
              (td))
          ,@(map render-fun-call (rest trace))))

;; render-prop-result/tc : property-result/tc -> xexpr
(define (render-prop-result/tc result)
  (match-define (property-result/tc fd-name p-name tc)
    result)
  (match-define (testcase in out) tc)
  `(tr ((class "danger"))
       (td ,(string-append (to-str fd-name) ":"
                           (to-str p-name)))
       (td ,(to-str in))
       (td)
       (td ,(to-str out))))

;; render-fun-call : fun-call -> xexpr
(define (render-fun-call fc)
  (match-define (fun-call fd in out) fc)
  `(tr ((class "warning"))
       (td ,(to-str fd))
       (td ,(to-str in))
       (td ,(to-str out))
       (td)))

;; to-str : any -> string
(define (to-str value)
  (format "~a" value))
(module+ test
  (check-pred string? (to-str 'game))
  (check-pred string? (to-str "joy"))
  (check-pred string? (to-str 45))
  (check-pred string? (to-str '(λ () (random)))))

;; to-racket : any/c -> racket
(define (to-racket value)
  (call-with-input-string value read))
(module+ test
  (check-pred number? (to-racket "42"))
  (check-pred symbol? (to-racket "joy"))
  (check-pred string? (to-racket "\"joy\""))
  (check-pred (λ (l)
                (and (list? l)
                     (= 2 (length l))
                     (= 3 (first l))
                     (equal? (second l) "joy"))) 
              (to-racket "(3 \"joy\")")))

(module+ main
  (serve/servlet
   grip-dispatch
   #:command-line? #t
   #:banner? #t
   #:port 2991
   #:servlet-regexp #rx""
   #:extra-files-paths (list htdocs)))
