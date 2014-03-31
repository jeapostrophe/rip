#lang racket/base

(require racket/list
         racket/match
         rackunit
         "model.rkt"
         "interact.rkt")

;; STRUCTS

;; param : symbol? (list info?)
(struct param (type info))
;; info
(struct info () #:transparent)
;; i-range : number? number?
(struct i-range info (start end) #:transparent)
;; i-predicate : procedure?
(struct i-predicate info (fun) #:transparent)
;; i-generator : procedure?
(struct i-generator info (fun) #:transparent)


;; DEFINES
(define min-char-int 0)
(define max-char-int 1114111)
(define lo-max-char-int 55295)
(define hi-min-char-int 57344)

(define min-char #x0) 
(define max-char #x10FFFF)
(define lo-max-char #xD7FF)
(define hi-min-char #xE000)



;; VALID QUICK-CHECK TYPES

;; valid-types : (list symbols)
;; all of the valid parameter types supported by quick-check
(define valid-types 
  (list 'bool 'char 'string 'symbol 'procedure 'integer 'double))

;; pos-num-range? : (S E -> i-range)
;; verfiy that start and end span a range of positive integers 
(define (pos-num-range? s e)
  (if (and (integer? s)
           (integer? e)
           (or (zero? s)
               (positive? s))
           (or (zero? e)
               (positive? e))
           (<= s e))
      (i-range s e)
      #f))

;; valid-ranges : (S E -> (i-range | bool))
(define (valid-ranges type)
  (case type 
    ['bool (λ (s e) 
             #f)]
    ['char (λ (s e) 
             (cond 
               [(and (char? s)
                     (char? e)
                     (char<=? min-char s lo-max-char) 
                     (char<=? hi-min-char e max-char)
                     (char<=? s e))
                (i-range (char->integer s) (char->integer e))]
               [(and (and (exact-integer? s)
                          (exact-integer? e)
                          (or (<= min-char-int s lo-max-char-int)
                              (<= hi-min-char-int s max-char-int))
                          (or (<= min-char-int e lo-max-char-int)
                              (<= hi-min-char-int e max-char-int))
                          (<= s e)))
                (i-range s e)]                 
               [else
                #f]))]
    ['string 
     pos-num-range?]
    ['symbol 
     pos-num-range?]
    ['procedure (λ (s e)
                  #f)]
    ['integer (λ (s e)
                (if (and (integer? s)
                         (integer? e)
                         (<= s e))
                    (i-range s e)
                    #f))]
    ['double (λ (s e)
               (if (and (number? s)
                        (number? e)
                        (<= s e))
                   (i-range s e)
                   #f))])) 



;; DEFAULTS

;; default-generator : symbol -> (range -> B)
(define (default-generator type)
  (case type
    ['bool random-bool]
    ['char random-char]
    ['string random-string]
    ['symbol random-symbol]
    ['procedure prompt-for-fun]
    ['integer random-int]
    ['double random-double]))

;; default-range : symbol -> range
(define (default-range type) 
  (case type
    ['bool (i-range 0 1)]
    ['char (i-range min-char-int max-char-int)]
    [('string 'symbol) (i-range 0 20)]                    
    ['procedure #f]
    ['integer (i-range -1000 1000)]
    ['double (i-range -1000 1000)]))


;; add-param-info : param info -> param
(define (add-param-info param-type info)
  (define prev-info (param-info param-type))
  (struct-copy param param-type
               [info
                (cons info 
                      (cond 
                        [(or (i-range? info)
                             (i-predicate? info))
                         (define existing-info 
                           (cond 
                             [(i-range? info)
                              (filter i-range? prev-info)]
                             [(i-predicate? info)
                              (filter i-predicate? prev-info)]))
                         (if (empty? existing-info)
                             prev-info
                             (remove existing-info prev-info))]
                        [else
                         prev-info]))]))

;; random-int : i-range -> integer
(define (random-int range)
  (match-define (i-range s e) range)
  (+ s (random (- (+ 1 e) s))))

;; random-double : i-range -> double
(define (random-double range)
  (match-define (i-range s e) range)
  (+ s (* (- e s) (random))))

;; random-bool : i-range -> bool
(define (random-bool range)
  (if (zero? ((default-generator 'integer) 
              (i-range 0 1)))
      #f
      #t))

;; random-char : i-range -> char
(define (random-char range)
  (integer->char 
   (let ()
     (define (valid-char int)
       (if (< lo-max-char-int int hi-min-char-int)
           (valid-char ((default-generator 'integer) range))
           int))
     (valid-char (random-int range)))))

;; random-string : i-range -> string
(define (random-string range)
  (list->string 
   (for/list ([i (in-range 
                  ((default-generator 'integer) range))])
     ((default-generator 'char) 
      (i-range 97 122)))))

;; random-symbol : i-range -> string
(define (random-symbol range)
  (string->symbol 
   ((default-generator 'string) range)))

;; prompt-for-fun : - -> symbol
(define (prompt-for-fun)
  (printf "Please enter the name of the function:")
  (read))



;; GETTERS

(define (get-valid-param)
  (printf "\nEnter param type:")
  (define type (read))
  (cond 
    [(and (symbol? type)
          (member type valid-types))
     (define p (param type empty))
     (add-info p)]
    [else
     (printf "Must be in the list of valid types: ")
     (for ([v-type valid-types])
       (printf v-type))
     (get-valid-param)]))

(define (get-valid-range fun)
  (printf "Enter the first value in the range")
  (define start (read))
  (printf "Enter the last value in the range")
  (define end (read))
  (define range (fun start end))
  (cond 
    [(not range)
     (print "Must be a valid range satisfying: ~a\n" fun)
     (get-valid-range)]
    [else
     range]))



;; SETTERS

;; add-info : param -> info
(define (add-info p)
  (define info (interact
                ["Enter a range of values"
                 (get-valid-range (valid-ranges (param-type p)))]
                ["Enter a predicate that the value must satisfy"
                 (i-predicate (read))]
                ["Enter a generator function"
                 (i-generator (read))]
                ["Exit"
                 #f]))
  (cond 
    [(info? info)
     (add-info (add-param-info (param-type p) info))]
    [else
     p]))



;; CUSTOM GENERATOR

;; param-generator : param -> ( -> B)
(define (param-generator p)
  (match-define (param p-type p-info) p)
  (define generator (filter i-generator? p-info))
  (cond 
    [(empty? generator)
     (define range (filter i-range? p-info))
     (define real-range
       (if (empty? range)
           (default-range p-type)
           range))
     (define fun (default-generator p-type))
     (if (equal? 'procedure p-type)
         fun
         (λ () (fun real-range)))]      
    [else 
     (first generator)]))

;; custom-generator : fun-defn -> ( -> list?)
(define (custom-generator fd)
  (printf "\nHow many parameters are in the function?")
  (define count (read))
  (cond 
    [(and (real? count)
          (positive? count))
     (define list-of-gens 
       (for/list ([i (in-range count)])
         ((param-generator (get-valid-param)))))
     (λ ()
       list-of-gens)]
    [else
     (printf "Invalid number\n")
     (custom-generator fd)]))


(define f1
  (fun-defn 'f1 
            (λ (x y) (+ x y)) 
            null
            (list (testcase (list 2 3) 5)
                  (testcase (list 2 -8) -6)
                  (testcase (list 0 1) 1))
            (hasheq)))

(define f2 
  (fun-defn 'f2 
            (λ (x) (* x x x))
            null
            (list (testcase (list 2) 8)
                  (testcase (list 3) 9)
                  (testcase (list -3) -9)
                  (testcase (list 0) 0))
            (hasheq 'increasing 
                    (λ (x) (> (f2 x) x))
                    'super-increasing 
                    (λ (x) (> (f2 x) (* x x))))))



;; TESTS

;; pos-num-range?
(check-equal? (pos-num-range? -1 12) #f)
(check-equal? (pos-num-range? -1000 -10) #f)
(check-equal? (pos-num-range? 0 12) (i-range 0 12))
(check-equal? (pos-num-range? 48 1200) (i-range 48 1200))

;; valid-ranges
(check-equal? ((valid-ranges 'integer) -3 4) (i-range -3 4))
(check-equal? ((valid-ranges 'integer) -3.5 4) #f)
(check-equal? ((valid-ranges 'integer) -3 -4) #f)
(check-equal? ((valid-ranges 'string) -10 10) #f)
(check-equal? ((valid-ranges 'string) 9 10.6) #f)
(check-equal? ((valid-ranges 'symbol) 0 99) (i-range 0 99))
(check-equal? ((valid-ranges 'char) 100 2000) (i-range 100 2000))
(check-equal? ((valid-ranges 'char) 100 140000) (i-range 100 140000))
(check-equal? ((valid-ranges 'char) 14000 90) #f)
(check-equal? ((valid-ranges 'char) 57300 57335) #f)
(check-equal? ((valid-ranges 'double) 300 -.9) #f)
(check-equal? ((valid-ranges 'double) -1 7093) (i-range -1 7093))

(provide
 custom-generator)
