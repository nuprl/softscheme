;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing

; parse a top-level definition
(define parse-def
  (lambda (def)
    (let ((parse-name
           (match-lambda
             ((? symbol? s)
              (if (keyword? s)
                  (syntax-error def "invalid use of keyword ~a" s)
                  s))
             (n (syntax-error def "invalid variable at ~a" n)))))
      (match def
        (('extend-syntax ((? symbol? name) . _) . _)
         (printf "Note: installing but _not_ checking (extend-syntax (~a) ...)~%" name)
         (eval def)
         ())
        (('extend-syntax . _)
         (syntax-error def "invalid syntax"))
        (('defmacro (? symbol? name) . _)
         (printf "Note: installing but _not_ checking (defmacro ~a ...)~%" name)
         (eval def)
         ())
        (('defmacro . _)
         (syntax-error def "invalid syntax"))
        (('define-primitive (? symbol? x) texp)
         (let ((x (parse-name x)))
           (defprim x texp 'impure)
           ()))
        (('define-primitive (? symbol? x) texp (? symbol? mode))
         (let ((x (parse-name x)))
           (defprim x texp mode)
           ()))
        (('define-primitive . _)
         (syntax-error def "invalid syntax"))
        (('define (? symbol? n) e)
         (list (make-Define (parse-name n) (box (parse-exp e)))))
        (('define (n . args) . body)
         (list (make-Define
                 (parse-name n)
                 (box (parse-exp `(lambda ,args ,@body))))))
        (('define . _)
         (syntax-error def "at define"))
        (('begin . defs)
         (foldr append () (smap parse-def defs)))
        (('define-structure (n . args))
         (parse-def `(define-structure (,n ,@args) ())))
        (('define-structure (n . args) inits)
         (let ((m-args (smap (lambda (x) `(! ,x)) args))
               (m-inits (smap (match-lambda
                                ((x e) `((! ,x) ,e))
                                (_ (syntax-error def
                                     "invalid structure initializer")))
                              inits)))
           (parse-def `(define-const-structure (,n ,@m-args) ,m-inits))))
        (('define-const-structure ((? symbol? n) . args))
         (parse-def `(define-const-structure (,n ,@args) ())))
        (('define-const-structure ((? symbol? n) . args) ())
         (let* ((parse-arg
                 (match-lambda
                   (('! '_) (list None None #t))
                   (('! a) (let ((a (parse-name a)))
                             (list (Some (symbol-append n '- a))
                                   (Some (symbol-append 'set- n '- a '!))
                                   #t)))
                   ('_ (list None None #f))
                   (a (let ((a (parse-name a)))
                        (list (Some (symbol-append n '- a))
                              None
                              #f)))))
                (arg-info (smap parse-arg args)))
           (list (make-Defstruct
                   n
                   (cons n args)
                   (symbol-append 'make- n)
                   (symbol-append n '?)
                   (map car arg-info)
                   (map cadr arg-info)
                   (map caddr arg-info)))))
        (('define-const-structure ((? symbol? n) . args) inits)
         (syntax-error def "sorry, structure initializers are not supported"))
        (('datatype . d)
         (let* ((parse-variant
                 (match-lambda
                   (((? symbol? con) . (? list? args))
                    (let ((n (parse-name con)))
                      (make-Variant
                        (symbol-append 'make- n)
                        (symbol-append n '?)
                        (cons con args))))
                   (_ (syntax-error def "invalid datatype syntax"))))
                (parse-dt
                 (match-lambda
                   (((? symbol? type) . variants)
                    (cons
                      (list (parse-name type))
                      (smap parse-variant variants)))
                   ((((? symbol? type) . (? list? targs)) . variants)
                    (cons
                      (cons (parse-name type) (smap parse-name targs))
                      (smap parse-variant variants)))
                   (_ (syntax-error def "invalid datatype syntax")))))
           (list (make-Datatype (smap parse-dt d)))))
        (((? symbol? k) . _)
         (if (and (not (keyword? k)) (macro? k))
             (parse-def (expand-once def))
             (list (make-Define #f (box (parse-exp def))))))
        (_ (list (make-Define #f (box (parse-exp def)))))))))

(define parse-exp
  (lambda (expression)
    (letrec
      ((n-primitive (string->symbol "#primitive"))
       (parse-exp
        (match-lambda
          (('quote (? symbol? s))    (make-Const s 'symbol?))
          ((and m ('quote _))        (parse-exp (quote-tf m)))
          ((and m ('quasiquote _))   (parse-exp (quasiquote-tf m)))
          ((and m (? box?))          (parse-exp (quote-tf m)))
          ((and m (? vector?))       (parse-exp (quote-tf m)))
          ((and m ('cond . _))       (parse-exp (cond-tf m)))
          ((and m ('case . _))       (parse-exp (case-tf m)))
          ((and m ('do . _))         (parse-exp (do-tf m)))
          ((? symbol? s)             (make-Var (parse-name s)))
          (#t                        (make-Const #t 'true-object?))
          (#f                        (make-Const #f 'false-object?))
          ((? null? c)               (make-Const c 'null?))
          ((? number? c)             (make-Const c 'number?))
          ((? char? c)               (make-Const c 'char?))
          ((? string? c)             (make-Const c 'string?))
          ((': ty e1)
           (make-Cast ty (parse-exp e1)))
          ((and exp ('record . bind))
           (let ((bindings (smap parse-bind bind)))
             (no-repeats (map Bind-name bindings) exp)
             (make-Record bindings)))
          ((and exp ('field name e1))
           (make-Field (parse-name name) (parse-exp e1)))
;          ((and exp ('module names . defs))
;           (let ((names (smap parse-name names)))
;             (no-repeats names exp)
;             (make-Module
;               names
;               (foldr append () (smap parse-def defs)))))
;          ((and exp ('import imports . body))
;           (let* ((parse-ibinding
;                   (match-lambda
;                     ((? symbol? s)
;                      (let ((s (parse-name s))) (cons s s)))
;                     (((? symbol? i) (? symbol? e))
;                      (cons (parse-name i) (parse-name e)))
;                     (_ (syntax-error exp "invalid import binding"))))
;                  (parse-import
;                   (match-lambda
;                     ((mod . bindings)
;                      (make-Iclause (parse-exp mod)
;                        (smap parse-ibinding bindings)))
;                     (_ (syntax-error exp "invalid import clause"))))
;                  (imp (smap parse-import imports)))
;             (no-repeats
;               (foldr append () (map
;                                  (match-lambda (($ Iclause _ bindings)
;                                                 (map cdr bindings)))
;                                  imp))
;               exp)
;             (make-Import
;               imp
;               (parse-body body))))
          ((and exp ('match e clause0 . clauses))
           (let* ((e2 (parse-exp e))
                  (parse-clause
                   (match-lambda
                     ((p ('=> (? symbol? failsym)) . body)
                      (make-Mclause
                        (parse-pat p expression)
                        (parse-body `((let ((,failsym (lambda () (,failsym))))
                                        ,@body)))
                        failsym))
                     ((p . body)
                      (make-Mclause
                        (parse-pat p expression)
                        (parse-body body)
                        #f))
                     (_ (syntax-error exp "invalid match clause")))))
             (make-Match e2 (smap parse-clause (cons clause0 clauses)))))
          ((and exp ('lambda bind . body))
           (recur loop ((b bind)(names ()))
             (match b
               ((? symbol? n)
                (let ((rest (parse-name n)))
                  (no-repeats (cons rest names) exp)
                  (make-Vlam (reverse names) rest (parse-body body))))
               (()
                (no-repeats names exp)
                (make-Lam (reverse names) (parse-body body)))
               ((n . x)
                (loop x (cons (parse-name n) names)))
               (_
                (syntax-error exp "invalid lambda expression")))))
          (('if e1 e2 e3)
           (make-If (parse-exp e1) (parse-exp e2) (parse-exp e3)))
          ((and if-expr ('if e1 e2))
           (printf "Note: one-armed if: ")
           (print-context if-expr 2)
           (make-If (parse-exp e1) (parse-exp e2) (parse-exp '(void))))
          (('delay e)
           (make-Delay (parse-exp e)))
          (('set! n e)
           (make-Set! (parse-name n) (parse-exp e)))
          (('and . args)
           (make-And (smap parse-exp args)))
          (('or . args)
           (make-Or (smap parse-exp args)))
          ((and exp ('let (? symbol? n) bind . body))
           (let* ((nb (parse-name n))
                  (bindings (smap parse-bind bind)))
             (no-repeats (map Bind-name bindings) exp)
             (make-App
               (make-Letr (list
                            (make-Bind
                              nb
                              (make-Lam (map Bind-name bindings)
                                (parse-body body))))
                 (make-Body () (list (make-Var nb))))
               (map Bind-exp bindings))))
          ((and exp ('let bind . body))
           (let ((bindings (smap parse-bind bind)))
             (no-repeats (map Bind-name bindings) exp)
             (make-Let bindings (parse-body body))))
          (('let* bind . body)
           (make-Let* (smap parse-bind bind) (parse-body body)))
          ((and exp ('letrec bind . body))
           (let ((bindings (smap parse-bind bind)))
             (no-repeats (map Bind-name bindings) exp)
             (make-Letr bindings (parse-body body))))
          (('begin e1 . rest)
           (make-Begin (smap parse-exp (cons e1 rest))))
          (('define . _)
           (syntax-error expression "invalid context for internal define"))
          (('define-structure . _)
           (syntax-error expression "invalid context for internal define-structure"))
          (('define-const-structure . _)
           (syntax-error expression "invalid context for internal define-const-structure"))
          ((and m (f . args))
           (cond ((and (eq? f n-primitive)
                       (match args
                         (((? symbol? p)) (make-Prim p))
                         (_ #f))))
                 ((and (symbol? f) (not (keyword? f)) (macro? f))
                  (parse-exp (expand-once m)))
                 (else
                  (make-App (parse-exp f) (smap parse-exp args)))))
          (x (syntax-error expression "invalid expression at ~a" x))))
       (parse-name
        (match-lambda
          ((? symbol? s)
           (when (keyword? s)
             (syntax-error expression "invalid use of keyword ~a" s))
           s)
          (n (syntax-error expression "invalid variable at ~a" n))))
       (parse-bind
        (match-lambda
          ((x e) (make-Bind (parse-name x) (parse-exp e)))
          (b (syntax-error expression "invalid binding at ~a" b))))
       (parse-body
        (lambda (body)
          (recur loop ((b body)(defs ()))
            (match b
              (((and d ('define . _)) . rest)
               (loop rest (append defs (parse-def d))))
              (((and d ('define-structure . _)) . rest)
               (loop rest (append defs (parse-def d))))
              (((and d ('define-const-structure . _)) . rest)
               (loop rest (append defs (parse-def d))))
              ((('begin) . rest)
               (loop rest defs))
              (((and beg ('begin ('define . _) . _)) . rest)
               (loop rest (append defs (parse-def beg))))
              (((and beg ('begin ('define-structure . _) . _)) . rest)
               (loop rest (append defs (parse-def beg))))
              (((and beg ('begin ('define-const-structure . _) . _)) . rest)
               (loop rest (append defs (parse-def beg))))
              ((_ . _)
               (make-Body defs (smap parse-exp b)))
              (_
               (syntax-error expression "invalid body at ~a" b))))))
       (no-repeats
        (lambda (l exp)
          (match l
            (() #f)
            ((_) #f)
            ((x . l)
             (if (memq x l)
                 (syntax-error exp "name ~a repeated" x)
                 (no-repeats l exp)))))))
      (parse-exp expression))))

(define parse-pat
  (lambda (pat expression)
    (letrec
      ((parse-pat
        (match-lambda
          (#f                     (make-Ppred 'false-object?))
          (#t                     (make-Ppred 'true-object?))
          (()                     (make-Ppred 'null?))
          ((? number? c)          (make-Pconst c 'number?))
          ((? char? c)            (make-Pconst c 'char?))
          ((? string? c)          (make-Pconst c 'string?))
          (('quote x)             (parse-quote x))
          ('_                     (make-Pany))
          ('else                  (make-Pelse))
          ((? symbol? n)          (make-Pvar (parse-pname n)))
          (('not . pats)          (syntax-error expression
                                    "not patterns are not supported"))
          (('or . pats)           (syntax-error expression
                                    "or patterns are not supported"))
          (('get! . pats)         (syntax-error expression
                                    "get! patterns are not supported"))
          (('set! . pats)         (syntax-error expression
                                    "set! patterns are not supported"))
          (('and . pats)
           (match (smap parse-pat pats)
             (((? Pvar?) (? Pvar?))
              (syntax-error expression
                "and-pattern has no non-variable branch"))
             (((? Pvar? a) b)
              (make-flat-Pand (list b a)))
             ((a (? Pvar? b))
              (make-flat-Pand (list a b)))
             (_ (syntax-error expression
                  "and-pattern must have two branches"))))
          (('? (? symbol? pred) p)
           (parse-pat `(and (? ,pred) ,p)))
          (('? (? symbol? pred))
           (if (keyword? pred)
               (syntax-error expression "invalid use of keyword ~a" pred)
               (make-Ppred pred)))
          (('$ (? symbol? c) . args)
           (if (memq c '(? _ $ and or not set! else))
               (syntax-error expression
                 "invalid use of pattern keyword ~a" c)
               (make-Pobj (symbol-append c '?) (smap parse-pat args))))
          ((? box? cb)
            (make-Pobj 'box? (list (parse-pat (unbox cb)))))
          ((x . y)
           (make-Pobj 'pair? (list (parse-pat x) (parse-pat y))))
          ((? vector? v)
           (make-Pobj 'vector? (map parse-pat (vector->list v))))
          (m (syntax-error expression "invalid pattern at ~a" m))))
       (parse-quote
        (match-lambda
          (#f                     (make-Pobj 'false-object? ()))
          (#t                     (make-Pobj 'true-object? ()))
          (()                     (make-Pobj 'null? ()))
          ((? number? c)          (make-Pconst c 'number?))
          ((? char? c)            (make-Pconst c 'char?))
          ((? string? c)          (make-Pconst c 'string?))
          ((? symbol? s)          (make-Pconst s 'symbol?))
          ((? box? cb)
            (make-Pobj 'box? (list (parse-quote (unbox cb)))))
          ((x . y)
           (make-Pobj 'pair? (list (parse-quote x) (parse-quote y))))
          ((? vector? v)
           (make-Pobj 'vector? (map parse-quote (vector->list v))))
          (m (syntax-error expression "invalid pattern at ~a" m))))
       (parse-pname
        (match-lambda
          ((? symbol? s)
           (cond ((keyword? s)
                  (syntax-error expression "invalid use of keyword ~a" s))
                 ((memq s '(? _ else $ and or not set! get! ... ..0 ..1 ..2 ..3))
                  (syntax-error expression "invalid use of pattern keyword ~a" s))
                 (else s)))
          (n (syntax-error expression "invalid pattern variable at ~a" n)))))
      (parse-pat pat))))

; a careful version of map that takes s-expressions as input
(define smap
  (lambda (f l)
    (match l
      (() ())
      ((x . r) (let ((v (f x))) (cons v (smap f r))))
      (_ (syntax-error l "invalid list")))))

(define primitive (lambda (p) (list (string->symbol "#primitive") p)))

(define keyword?
  (lambda (s)
    (memq
      s
      '(=> and begin case cond do define delay if lambda let let* letrec
           or quasiquote quote set! unquote unquote-splicing
           match define-structure define-const-structure
           record field : datatype))))

(define make-flat-Pand
  (lambda (pats)
    (let ((l (foldr
               (lambda (p plist)
                 (match p
                   (($ Pand pats) (append pats plist))
                   (_ (cons p plist))))
               ()
               pats)))
      (match l
        (() (make-Pelse))
        ((p) p)
        (_ (make-Pand l))))))
