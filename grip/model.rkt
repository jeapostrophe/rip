#lang racket/base
(require racket/contract)

(define-syntax-rule (struct/ctc name ([field ctc] ...))
  (begin (struct name (field ...) #:transparent)
         (provide (contract-out (struct name ((field ctc) ...))))))

;; fun-defn : symbol (A -> B) (list symbol) 
;;            (list (testcase A B)) (hasheq symbol (property A B))
;;(struct fun-defn (name code param-types test-cases properties))
(struct/ctc fun-defn ([name symbol?] 
                      [code procedure?] 
                      [generator (or/c null?
                                       procedure?)] 
                      [test-cases list?] 
                      [properties hash?]))
;; testcase : (list values) value
;;(struct testcase (input output))
(struct/ctc testcase ([input list?] 
                      [output any/c]))
;; testcase-result : testcase (list fun-call)
;;(struct testcase-result (tc trace))
(struct/ctc testcase-result ([tc testcase?] 
                         [trace (non-empty-listof fun-call?)]))
;; property-result : string (list fun-call)
;;(struct property-result (property-name trace))
(struct/ctc property-result ([property-name symbol?] 
                             [trace (non-empty-listof fun-call?)]))
;; fun-call : fun-defn (list values) value
;;(struct fun-call (fd input output))
(struct/ctc fun-call ([fd fun-defn?] 
                  [input list?]
                  [output any/c]))
;; result : bool (list fun-call fd in out)  
;;(struct result (success trace))

(struct/ctc result ([success boolean?] 
                    [trace (non-empty-listof fun-call?)]))
