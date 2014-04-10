#lang web-server/insta

(require racket/list
         racket/match
         rackunit
         racket/string
         "model.rkt"
         "editor.rkt"
         "interp.rkt"
         "expr-based-qc.rkt"
         "custom-qc.rkt"
         "web-model.rkt")

;; xxx To Do List
;; - bug with toggling tabs inside of tabs
;; - editing function code
;; - adding/removing testcases
;; - adding/removing properties
;; - adding quick check
;; - making buttons in the results window look better?


;; START

(define (start request)
  (render-page request))



;; HANDLERS

(define (render-page request)  
  (local [(define (response-generator embed/url)
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
                          ,(render-fd-panel embed/url request)
                          ,(render-results-panel embed/url request)))))))]
    
    (send/suspend/dispatch response-generator)))

(static-files-path "htdocs")

(define (insert-function-handler request)          
  (insert-fd! 
   FDs (parse-fun-defn (request-bindings request)))
  (set-results-rs! RESULTs empty)
  (render-page (redirect/get)))

;; parse-fun-defn : bindings -> fun-defn
(define (parse-fun-defn bindings)
  (fun-defn (string->symbol (extract-binding/single 'name bindings))
            '(λ () )
            empty
            empty
            (hasheq)))

(define (run-tcs-handler request)
  (set-results-rs! RESULTs 
                   (test-fun (program-fun-defns FDs)
                             (string->symbol 
                              (extract-binding/single 'fun-name 
                                                      (request-bindings request)))))
  (render-page (redirect/get)))

(define (test-prop-handler request)
  ;; xxx get quick check working
  (render-page (redirect/get)))

(define (gen-worklist-handler request)
  (set-results-rs! RESULTs
                   (gen-worklist (program-fun-defns FDs)))
  (render-page (redirect/get)))



;; FUNCTION DEFINITIONS

;; render-fd-panel : embed/url request -> xexpr
(define (render-fd-panel embed/url request)
  `(div ((class "panel panel-primary"))
        (div ((class "panel-heading"))
             (h3 ((class "panel-title"))
                 "Program"))        
        (div ((class "panel-body"))
        (ul ((class "nav nav-tabs")
             (style "margin-bottom: 15px;")) 
            ,@(map render-fd-tab 
                   (hash-values (program-fun-defns FDs)))
            (li (a ((href "#new-fd")
                    (data-toggle "tab"))
                   "+ Function")))
        (div ((class "tab-content"))
             ,@(map render-fd-content 
                    (hash-values (program-fun-defns FDs)))             
             ,(render-new-fd-form embed/url)))))

;; render-new-fd-form : embed/url handler -> xexpr
(define (render-new-fd-form embed/url)
  `(div ((class "tab-pane fade")
         (id "new-fd"))
        (form ((class "form-horizontal")
               (action 
                ,(embed/url insert-function-handler)))
              (fieldset 
               (legend "Enter information for a new function")
               (div ((class "form-group"))
                    (label ((for "input-name")
                            (class "col-lg-2 control-label"))
                           "Name")
                    (div ((class "col-lg-10"))
                         (input ((class "form-control")
                                 (id "input-name")
                                 (placeholder "Name")
                                 (name "name")
                                 (type "text")))))
               (div ((class "form-group"))
                    (div ((class "col-lg-10 col-lg-offset-2"))
                         (button ((class "btn btn-primary")
                                  (type "submit"))
                                 "Sumbit")))))))

;; render-fd-tab : fun-defn -> xexpr
(define (render-fd-tab fd)
  (match-define (fun-defn name code generator tcs props) fd)
  (define tab-id (string-append "#"
                                (symbol->string name)))
  `(li (a ((href ,tab-id)
           (data-toggle "tab"))
          ,(symbol->string name))))


;; render-fd-content : fun-defn -> xexpr
(define (render-fd-content fd)
  (match-define (fun-defn name code generator tcs props) fd)
  (define tab-id (string-append "#" (symbol->string name)))
  `(div ((class "tab-pane fade")
         (id ,(symbol->string name)))
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
                   (id ,(string-append (symbol->string name) "-code")))
                  (textarea ,(to-string code))))
        (div ((class "tab-content"))
             (div ((class "tab-pane fade")
                   (id ,(string-append (symbol->string name) "-testcases")))
                  ,(render-testcases tcs)))
        (div ((class "tab-content"))
             (div ((class "tab-pane fade")
                   (id ,(string-append (symbol->string name) "-properties")))
                  ,(render-properties props)))))

;; render-testcases : (list testcase) -> xexpr
(define (render-testcases tcs) 
  `(table ((class "table"))
          (thead
           (tr (th "Input")
               (th "Output")))
          (tbody ,@(map render-testcase tcs))))

;; render-testcase : testcase
(define (render-testcase tc)
  (match-define (testcase in out) tc)
  `(tr (td ,(to-string in)) (td ,(to-string out))))

;; render-properties : hasheq
(define (render-properties props) 
  (if (zero? (hash-count props)) 
      `(div ((class "alert alert-dimissable alert-info"))
            (button ((type "button")
                     (class "close")
                     (data-dismiss "alert"))
                     "x")
            "Currently, there are no properties for this function.")
      `(table ((class "table"))
          (thead
           (tr (th "Name")
               (th "Function")))
          (tbody ,@(hash-map props render-property)))))

;; render-property : string any
(define (render-property name fun)
  `(tr (td ,(symbol->string name)) 
       (td ,(to-string fun))))



;; RESULTS

;; render-results-panel : results -> xexpr
(define (render-results-panel embed/url request)
  `(div ((class "panel panel-info"))
        (div ((class "panel-heading"))
             (h3 ((class "panel-title"))"Results"))
        (div ((class "panel-body"))
             ,(render-results embed/url request)
             (div ((class "well well-sm"))
                  (form ((action 
                          ,(embed/url run-tcs-handler)))
                        (div ((class "form-group"))
                             (div ((class "input-group"))
                                  (span ((class "input-group-addon"))
                                        "Function Name:")
                                  (input ((class "form-control")
                                          (type "text")
                                          (name "fun-name")))
                                  (span ((class "input-group-btn"))
                                        (button ((class "btn btn-default")
                                                 (type "submit"))
                                                "Run Test Cases")))))
                  (form ((action
                          ,(embed/url test-prop-handler)))
                        (button ((class "btn btn-default")
                                 (type "submit"))
                                "Test a Property"))
                  (form ((action
                          ,(embed/url gen-worklist-handler)))
                        (button ((class "btn btn-default")
                                 (type "submit"))
                                "Generate Work List"))))))

;; render-results : (list testcase-result) -> xexpr
(define (render-results embed/url request)
  (define rs (results-rs RESULTs))
  (if (empty? rs)
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
                     rs))))

;; render-tc-result : testcase-result -> xexpr
(define (render-tc-result result)
  (match-define (testcase-result tc trace) 
    result)
  (match-define (fun-call fd in out) 
    (first trace))
  `(tbody (tr ((class "danger"))
              (td ,(symbol->string fd))
              (td ,(to-string in))
              (td ,(to-string out))
              (td ,(to-string (testcase-output tc))))
          ,@(map render-fun-call (rest trace))))

;; render-prop-result : property-result -> xexpr
(define (render-prop-result result)
  (match-define (property-result p-name trace) 
    result)
  (match-define (fun-call fd in out) 
    (first trace))
  `(tbody (tr ((class "danger"))
              (td ,(string-append (symbol->string fd) ": "
                                  (symbol->string p-name)))
              (td ,(to-string in))
              (td ,(to-string out))
              (td))
          ,@(map render-fun-call (rest trace))))

;; render-prop-result/tc : property-result/tc -> xexpr
(define (render-prop-result/tc result)
  (match-define (property-result/tc fd-name p-name tc) 
    result)
  (match-define (testcase in out) tc)
  `(tr ((class "danger"))
       (td ,(string-append (symbol->string fd-name) ":"
                           (symbol->string p-name)))
       (td ,(to-string in))
       (td)
       (td ,(to-string out))))

;; render-fun-call : fun-call -> xexpr
(define (render-fun-call fc)
  (match-define (fun-call fd in out) fc)
  `(tr ((class "warning"))
       (td ,(symbol->string fd))
       (td ,(to-string in))
       (td ,(to-string out))
       (td)))

;; to-string : any -> string
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
