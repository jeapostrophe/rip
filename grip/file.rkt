#lang racket/base
(require racket/rerequire
         racket/contract/base
         racket/list
         racket/match
         racket/pretty
         "model.rkt")

(define (read-from-file! target p)
  (define the-bs (open-output-bytes))
  (parameterize ([current-output-port the-bs])
    (dynamic-rerequire `(submod ,p read)))
  (define ip (open-input-bytes (get-output-bytes the-bs)))
  (for ([we (in-port read ip)])
    (define e (second we))
    (hash-set! target (fun-defn-name e) e)))
(provide
 (contract-out
  [read-from-file!
   (-> (hash/c symbol? fun-defn?)
       path-string?
       void?)]))

(define (render-gen gen)
  (if (list? gen)
    (list '#:generator gen)
    empty))
(define (render-tests tcs)
  (if (empty? tcs)
    empty
    (cons '#:tests
          (for/list ([tc (in-list tcs)])
            (match-define (testcase i o) tc)
            (list i o)))))
(define (render-props props)
  (if (zero? (hash-count props))
    empty
    (cons '#:properties
          (for/list ([(n f) (in-hash props)])
            (list n f)))))
(define (render-fd fd)
  (match-define (fun-defn name code gen tcs props) fd)
  `(define-fun ,name
     ,code
     ,@(render-gen gen)
     ,@(render-tests tcs)
     ,@(render-props props)))
(define (write-out table)
  (for ([fd (in-hash-values table)])
    (pretty-write (render-fd fd))))
(module+ test
  (require racket/runtime-path)
  (define FUN-DEFNS (make-hash))
  (define-runtime-path example.rkt "example.rkt")
  (read-from-file! FUN-DEFNS example.rkt)
  (write-out FUN-DEFNS))
(define (write-to-file! table p)
  (with-output-to-file p 
    #:exists 'replace
    (λ ()
      (displayln "#lang racket/base")
      (displayln "(require \"lang.rkt\")")
      (newline)
      (write-out table))))
(provide
 (contract-out
  [write-to-file!
   (-> (hash/c symbol? fun-defn?)
       path-string?
       void?)]))
