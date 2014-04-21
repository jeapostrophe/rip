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
(define (has-fun-defn? name)
  (hash-has-key? FUN-DEFNS name))
(define (set-fun-defn! name new-fd)
  (hash-set! FUN-DEFNS name new-fd)
  (write-to-file! FUN-DEFNS example.rkt))
(define (has-prop? fd-name p-name)
  (hash-has-key? (fun-defn-properties (get-fun-defn fd-name))
                 p-name))
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

(define ERROR
  (box empty))
(define (get-errors)
  (unbox ERROR))
(define (add-error! msg)
  (set-box! ERROR 
            (cons msg (unbox ERROR))))
(define (clear-errors)
  (set-box! ERROR empty))

(provide
 (contract-out
  [get-worklist
   (-> (listof (or/c property-result?
                     testcase-result?)))]
  [get-fun-defns
   (-> (listof fun-defn?))]
  [has-prop?
   (-> symbol? symbol? boolean?)]
  [has-fun-defn?
   (-> symbol? boolean?)]
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
             (listof property-result?)))]
  [get-errors
   (-> list?)]
  [add-error!
   (-> string? void?)]
  [clear-errors (-> void?)]))
