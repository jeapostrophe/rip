#lang racket/base
(require racket/list
         racket/contract/base
         racket/runtime-path
         "file.rkt"
         "model.rkt"
         "editor.rkt")

;; xxx remove globals and make this whole file an object/struct and
;; allow selecting other
(define-runtime-path example.rkt "example.rkt")

(define FUN-DEFNS (make-hash))
(read-from-file! FUN-DEFNS example.rkt)

(define (get-fun-defns)
  (hash-values FUN-DEFNS))
(define (get-fun-defn name)
  (hash-ref FUN-DEFNS name))
(define (set-fun-defn! name new-fd)
  (hash-set! FUN-DEFNS name new-fd)
  (write-to-file! FUN-DEFNS example.rkt))
(define (get-worklist)
  (gen-worklist FUN-DEFNS))

(define QC-RESULTS
  (box #f))
(define (get-quick-check-results)
  (unbox QC-RESULTS))
(define (run-quick-check f p k)
  (set-box! QC-RESULTS
            (if (empty? (fun-defn-generator
                         (get-fun-defn
                          f)))
              (vector (symbol->string f)
                      (symbol->string p)
                      k)
              (quick-check FUN-DEFNS
                           f p k))))

(provide
 (contract-out
  [get-worklist
   (-> (listof testcase-result?))]
  [get-fun-defns
   (-> (listof fun-defn?))]
  [get-fun-defn
   (-> symbol? fun-defn?)]
  [set-fun-defn!
   (-> symbol? fun-defn?
       void?)]
  [run-quick-check
   (-> symbol? symbol? number?
       void?)]
  [get-quick-check-results
   (-> (or/c false/c
             (vector/c string? string? number?)
             (listof property-result?)))]))
