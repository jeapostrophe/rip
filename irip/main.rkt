#lang racket/base
(require racket/match
         racket/list)
(module+ test
  (require rackunit/chk))

(struct val () #:transparent)
(struct v:num val (n) #:transparent)
(struct v:bool val (b) #:transparent)
(struct v:null val () #:transparent)
(struct v:cons val (car cdr) #:transparent)
(struct v:prim val (id prim) #:transparent)
(struct v:clo val (args body env) #:transparent)
(struct v:abstract val (env expr) #:transparent)

(define parse-val
  (match-lambda
   [(? number? n)
    (v:num n)]
   [(? boolean? b)
    (v:bool b)]
   [(? null?)
    (v:null)]
   [(? cons? c)
    (v:cons (parse-val (car c))
            (parse-val (cdr c)))]))

(struct expr () #:transparent)
(struct e:val expr (v) #:transparent)
(struct e:lam expr (args body) #:transparent)
(struct e:if expr (c t f) #:transparent)
(struct e:app expr (fun args) #:transparent)
(struct e:id expr (id) #:transparent)
(struct e:abstract expr (label) #:transparent)

(define ->racket
  (match-lambda
   [(v:num n) n]
   [(v:bool b) b]
   [x x]))
(define racket->
  parse-val)

(define (prim-bin f)
  (λ (x y) (racket-> (f (->racket x) (->racket y)))))
(define (prim-una f)
  (λ (x) (racket-> (f (->racket x)))))

(define PRIMITIVES
  (hasheq
   '+ (prim-bin +)
   '- (prim-bin -)
   '* (prim-bin *)
   '/ (prim-bin /)
   'zero? (prim-una zero?)
   '= (prim-bin =)
   'not (prim-una not)
   'or (prim-bin (λ (x y) (or x y)))
   'and (prim-bin (λ (x y) (and x y)))
   'number? (compose racket-> v:num?)
   'boolean? (compose racket-> v:bool?)
   'null? (compose racket-> v:null?)
   'cons v:cons
   'cons? (compose racket-> v:cons?)
   'car v:cons-car
   'cdr v:cons-cdr
   'procedure? (λ (f) (racket-> (or (v:clo? f) (v:prim? f))))
   'check
   (λ (x y)
     (if (equal? x y)
       (v:bool #t)
       (error 'check "Failed: ~e vs ~e" x y)))))

(define MACROS
  (make-hasheq))

(define parse-expr
  (match-lambda
   [(list 'quote val)
    (e:val (parse-val val))]
   [(list 'λ (list (? symbol? args) ...) body)
    (e:lam args (parse-expr body))]
   [(list 'if c t f)
    (e:if (parse-expr c) (parse-expr t) (parse-expr f))]
   [(list '_ any)
    (e:abstract any)]
   [(and macro-invoke (list-rest (? (λ (id) (hash-has-key? MACROS id)) macro) _))
    (parse-expr ((hash-ref MACROS macro) macro-invoke))]
   [(list fun args ...)
    (e:app (parse-expr fun) (map parse-expr args))]
   [(? symbol? id)
    (if (hash-has-key? PRIMITIVES id)
      (e:val (v:prim id (hash-ref PRIMITIVES id)))
      (e:id id))]))

(define (v:false? v)
  (and (v:bool? v) (not (v:bool-b v))))

(define (v:apply prim-or-clo arg-vs ok fail)
  (define fe
    (e:app (e:val prim-or-clo)
           (map e:val arg-vs)))
  (match prim-or-clo
    [(v:clo arg-ids body env)
     (if (= (length arg-vs) (length arg-ids))
       (interp body
               (for/fold ([env env])
                   ([ai arg-ids]
                    [av arg-vs])
                 (hash-set env ai av))
               ok
               fail)
       (fail (format "Wrong number of arguments: ~e vs ~e"
                     (length arg-vs) (length arg-ids))
             fe))]
    [(v:prim _ prim)
     (let/ec outside
       (ok
        (with-handlers ([exn:fail?
                         (λ (x)
                           (outside (fail (exn-message x) fe)))])
          (apply prim arg-vs))))]))

(define (snoc l x)
  (append l (list x)))

;;   ok : val -> ans
;; fail : msg expr -> ans
(define (interp e env ok fail)
  (match e
    [(e:abstract label)
     (ok (v:abstract env (e:abstract label)))]
    [(e:id i)
     (if (hash-has-key? env i)
       (ok (hash-ref env i))
       (fail (format "Unknown identifier: ~e" i) (e:id i)))]
    [(e:val v)
     (ok v)]
    [(e:if c t f)
     (interp
      c env
      (λ (cv)
        (if (v:false? cv)
          (interp f env ok fail)
          (interp t env ok fail)))
      (λ (r ce)
        (fail r (e:if ce t f))))]
    [(e:lam args body)
     (ok (v:clo args body env))]
    [(e:app fun args)
     (interp
      fun env
      (λ (fv)
        (define (interp-args args-v args-e)
          (if (empty? args-e)
            (v:apply fv args-v ok fail)
            (interp (first args-e)
                    env
                    (λ (arg-v)
                      (interp-args (snoc args-v arg-v)
                                   (rest args-e)))
                    (λ (r arg-e)
                      (fail
                       r
                       (e:app (e:val fv)
                              (append
                               (map e:val args-v)
                               (cons arg-e (rest args-e)))))))))
        (interp-args empty args))
      (λ (r fe)
        (fail r (e:app fe args))))]))

(define (interp* se)
  (interp
   (parse-expr se)
   (hasheq)
   (λ (sev) sev)
   (λ (r see) (error 'fail "~a: ~e" r see))))

(module+ test
  (chk
   (interp* ''42) (v:num 42)
   (interp* ''#t) (v:bool #t)
   (interp* ''#f) (v:bool #f)
   (interp* ''()) (v:null)
   (interp* ''(1 2)) (v:cons (v:num 1) (v:cons (v:num 2) (v:null)))
   (interp* '(if '#t '0 '1)) (v:num 0)
   (interp* '(if '2 '0 '1)) (v:num 0)
   (interp* '(if '#f '0 '1)) (v:num 1)
   (interp* '(λ () '0)) (v:clo (list) (parse-expr ''0) (hasheq))
   (interp* '(λ (x) '0)) (v:clo (list 'x) (parse-expr ''0) (hasheq))
   (interp* '(λ (x) x)) (v:clo (list 'x) (parse-expr 'x) (hasheq))
   (interp* '((λ () '0))) (v:num 0)
   (interp* '((λ (x) '0) '4)) (v:num 0)
   (interp* '((λ (x) x) '4)) (v:num 4)

   (interp* '((λ (x) (λ () '0)) '4))
   (v:clo (list) (parse-expr ''0) (hasheq 'x (v:num 4)))

   (interp* '(((λ (x) (λ () x)) '4))) (v:num 4)

   (interp* '(+ '1 '2)) (parse-val '3)

   (interp* '(cons? '1)) (parse-val '#f)
   (interp* '(cons? '#t)) (parse-val '#f)
   (interp* '(cons? '())) (parse-val '#f)
   (interp* '(cons? '(1 2))) (parse-val '#t)

   (interp* '(cons '1 '2)) (parse-val '(1 . 2))
   (interp* '(car (cons '1 '2))) (parse-val '1)
   (interp* '(cdr (cons '1 '2))) (parse-val '2)
   (interp* '(cons? (cons '1 '2))) (parse-val '#t)))

(define-syntax-rule (define-macro! id fun)
  (hash-set! MACROS 'id fun))

(define-macro! let
  (match-lambda
   [(list 'let (list (list arg-id arg-expr) ...) body)
    (list* (list 'λ arg-id body)
           arg-expr)]))

(module+ test
  (chk
   (interp* '(let ([x '4]) x)) (parse-val '4)
   (interp* '(let ([x '1] [y '3]) (+ x y))) (parse-val '4)))

(define :Y
  `(λ (make-thing)
     (let ([y (λ (x)
                (make-thing (λ (v) ((x x) v))))])
       (y y))))

(define-macro! λ/rec
  (match-lambda
   [(list 'λ/rec fun-id args body)
    (list :Y
          (list 'λ (list fun-id)
                (list 'λ args body)))]))

(module+ test
  (define :sum
    `(λ/rec
      sum (l)
      (if (null? l)
        '0
        (+ (car l) (sum (cdr l))))))

  (chk
   (interp* `(,:sum '(1 2 3 4))) (parse-val '10)))

(define-macro! letrec
  (match-lambda
   [(list 'letrec (list (list fun-id fun-expr) ...) body)
    (list 'let (for/list ([fun-id fun-id] [fun-expr fun-expr])
                 (list fun-id
                       (list :Y
                             (list 'λ (list fun-id) fun-expr))))
          body)]))

(define-macro! begin
  (match-lambda
   [(list 'begin before ... last)
    (list 'let
          (for/list ([before before])
            (list (gensym 'begin) before))
          last)]))

(define (interactive se)
  (interp
   (parse-expr se)
   (hasheq)
   (λ (sev) sev)
   (λ (r see)
     (print-context see))))

(require racket/pretty)

(define unparse
  (match-lambda
   [(e:app fun args)
    (list* (unparse fun) (map unparse args))]
   [(e:lam args body)
    (list 'λ args (unparse body))]
   [(v:clo args body env)
    (list '<closure> (unparse env) args (unparse body))]
   [(v:prim id _)
    id]
   [(e:val (and (or (? v:clo?) (? v:prim?)) v))
    (unparse v)]
   [(e:val v)
    (list 'quote (unparse v))]
   [(v:cons a d)
    (cons (unparse a) (unparse d))]
   [(v:num n)
    n]
   [(v:null)
    null]
   [(v:bool b)
    b]
   [(v:abstract env e)
    (list '<abstract>
          (unparse env)
          (unparse e))]
   [(e:abstract id)
    (list '_ id)]
   [(e:id id)
    id]
   [(? hash? env)
    (for/list ([(k v) env])
      (list k '-> (unparse v)))]))

(define (print-context e)
  (pretty-display (unparse e)))

(module+ test
  (interactive
   `(letrec ([sum (λ (l) (_ sum-definition))])
      (begin (check (sum '(1 2 3 4)) '10)
             (check (sum '(1 1 1 1)) '4)
             (check (sum '(1 1 1 1 5 6)) '15)
             (check (sum '()) '0)
             (check (sum '(1)) '1)
             (check (sum '(2)) '2)
             (check (sum '(1 2)) '3)
             '#t))))
