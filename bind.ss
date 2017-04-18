;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lexical Binding
;;
;; The binding occurrence and all bound occurrences of a variable
;; are replaced by a single shared record.  The sharing represents
;; the binding information.
;;
;; These routines use state to avoid passing around the environment
;; under construction.

(define cons-is-mutable #f)
(define n-unbound 0)

(define bind-defs
  (lambda (defs)
    (set! cons-is-mutable #f)
    (set! n-unbound 0)
    (reinit-env!)
    (letrec
      ((unbound-env empty-env)
       (redefined-prims ())
       (bind
        (lambda (e env tenv context)
          (let ((bind-cur (lambda (x) (bind x env tenv context))))
            (match e
              (($ Var x)
               (make-Var (use-var env x context)))
              (($ Prim x)
               (make-Var (use-var initial-env x context)))
              (($ Const c pred)
               (make-Const c (use-var initial-env pred context)))
              (($ Lam args e2)
               (let* ((b-args (map bind-var args))
                      (newenv (extend-env* env args b-args)))
                 (make-Lam b-args (bind e2 newenv tenv context))))
              (($ Vlam args rest e2)
               (let* ((b-args (map bind-var args))
                      (b-rest (bind-var rest))
                      (newenv (extend-env* env
                                (cons rest args) (cons b-rest b-args))))
                 (make-Vlam b-args b-rest (bind e2 newenv tenv context))))
              (($ Match e1 clauses)
               (make-Match
                 (bind-cur e1)
                 (map (lambda (x) (bind-mclause x env tenv context)) clauses)))
              (($ App e1 args)
               (make-App (bind-cur e1) (map bind-cur args)))
              (($ Begin exps)
               (make-Begin (map bind-cur exps)))
              (($ And exps)
               (make-And (map bind-cur exps)))
              (($ Or exps)
               (make-Or (map bind-cur exps)))
              (($ If test then els)
               (make-If (bind-cur test) (bind-cur then) (bind-cur els)))
              (($ Delay e2)
               (make-Delay (bind-cur e2)))
              (($ Set! x e2)
               (let ((b (use-var env x context)))
                 (when (Name-struct b)
                   (syntax-error (pexpr e)
                     "define-structure identifier ~a may not be assigned" x))
                 (when (and (Name-primitive b) (not (memq b redefined-prims)))
                   (set! redefined-prims (cons b redefined-prims)))
                 (set-Name-mutated! b #t)
                 (make-Set! b (bind-cur e2))))
              (($ Let args e2)
               (let* ((b-args (map (match-lambda
                                     (($ Bind x e)
                                      (make-Bind (bind-var x) (bind-cur e))))
                                   args))
                      (newenv (extend-env* env
                                (map Bind-name args) (map Bind-name b-args))))
                 (make-Let b-args (bind e2 newenv tenv context))))
              (($ Let* args e2)
               (recur loop ((args args)(b-args ())(env env))
                 (match args
                   ((($ Bind x e) . rest)
                    (let ((b (bind-var x)))
                      (loop rest
                            (cons (make-Bind b (bind e env tenv context)) b-args)
                            (extend-env env x b))))
                   (() (make-Let* (reverse b-args) (bind e2 env tenv context))))))
              (($ Letr args e2)
               (let* ((b-args (map (match-lambda
                                     (($ Bind x e) (make-Bind (bind-var x) e)))
                                   args))
                      (newenv (extend-env* env
                                (map Bind-name args) (map Bind-name b-args)))
                      (b-args (map (match-lambda
                                     (($ Bind b e)
                                      (let* ((n (Name-occ b))
                                             (e2 (bind e newenv tenv context)))
                                        ; dont include self recursive refs
                                        (set-Name-occ! b n)
                                        (make-Bind b e2))))
                                   b-args)))
                 (make-Letr b-args (bind e2 newenv tenv context))))
              (($ Body defs exps)
               (match-let* (((defs newenv newtenv)
                             (bind-defn defs env tenv #f)))
                 (make-Body defs
                   (map (lambda (x) (bind x newenv newtenv context)) exps))))
              (($ Record args)
               (make-Record
                 (map (match-lambda
                        (($ Bind x e)
                         (new-field! x)
                         (make-Bind x (bind-cur e))))
                      args)))
              (($ Field x e2)
               (new-field! x)
               (make-Field x (bind-cur e2)))
              (($ Cast ty e2)
               (make-Cast (cons ty tenv) (bind-cur e2)))
;              (($ Module names defs)
;               (match-let* (((defs . newenv) (bind-defn defs env #f)))
;                 (make-Module
;                   (map (lambda (x) (use-var newenv x context)) names)
;                   defs)))
;              (($ Import iclauses body)
;               (let* ((b-cls (map (match-lambda
;                                    (($ Iclause exp bindings)
;                                     (make-Iclause
;                                       (bind-cur exp)
;                                       (map (match-lambda
;                                              (($ Bind i e)
;                                               (make-Bind i (bind-var e))))
;                                            bindings))))
;                                  iclauses))
;                      (newenv (extend-env* env
;                                (foldr append ()
;                                  (map (match-lambda
;                                         (($ Iclause _ bindings)
;                                          (map Bind-exp bindings)))
;                                       iclauses))
;                                (foldr append ()
;                                  (map (match-lambda
;                                         (($ Iclause _ bindings)
;                                          (map Bind-exp bindings)))
;                                       b-cls)))))
;                 (make-Import b-cls (bind body newenv tenv context))))
              ))))
       (bind-mclause
        (lambda (clause env tenv context)
          (match-let*
            ((($ Mclause pattern body failsym) clause)
             (patenv empty-env)
             (bp (recur loop ((p pattern))
                   (match p
                     (($ Pvar x)
                      (when (bound? patenv x)
                        (syntax-error (ppat pattern)
                          "pattern variable ~a repeated" x))
                      (let ((b (bind-var x)))
                        (set! patenv (extend-env patenv x b))
                        (make-Pvar b)))
                     (($ Pobj c args)
                      (let ((b (use-var env c context)))
                        (when (boolean? (Name-predicate b))
                          (syntax-error (ppat pattern)
                            "~a is not a predicate" c))
                        (make-Pobj b (map loop args))))
                     (($ Pand pats)
                      (make-Pand (map loop pats)))
                     (($ Pnot pat)
                      (make-Pnot (loop pat)))
                     (($ Ppred pred)
                      (let ((b (use-var env pred context)))
                        (unless (Name-predicate b)
                          (syntax-error (ppat pattern)
                            "~a is not a predicate" pred))
                        (make-Ppred b)))
                     (($ Pany) p)
                     (($ Pelse) p)
                     (($ Pconst c pred)
                      (make-Pconst c (use-var initial-env pred context)))))))
            (if failsym
                (let ((b (bind-var failsym)))
                  (when (bound? patenv failsym)
                    (syntax-error (ppat pattern)
                      "fail symbol ~a repeated" failsym))
                  (set! patenv (extend-env patenv failsym b))
                  (make-Mclause
                    bp
                    (bind body (join-env env patenv) tenv context)
                    b))
                (make-Mclause
                  bp
                  (bind body (join-env env patenv) tenv context)
                  #f)))))
       (bind-defn
        (lambda (defs env tenv glob)
          (let*
            ((newenv empty-env)
             (newtenv empty-env)
             (struct-def (lambda (x pure)
                           (when (or (bound? newenv x)
                                     (and glob (bound? initial-env x)))
                             (syntax-error #f "~a defined more than once" x))
                           (let ((b (bind-var x)))
                             (set-Name-primitive! b #t)
                             (set-Name-struct! b #t)
                             (set-Name-subtyped! b #t)
                             (set-Name-pure! b pure)
                             (set! newenv (extend-env newenv x b))
                             b)))
             (bind1 (match-lambda
                      ((and z ($ Define x box-e))
                       (cond ((not x)
                              z)
                             (glob
                              (if (or (bound? newenv x) (bound? initial-env x))
                                  (let ((b (lookup-or-fail newenv x
                                             (lambda ()
                                               (lookup initial-env x)))))
                                    (make-Define #f
                                      (box (make-Set! x (unbox box-e)))))
                                  (let ((b (bind-var x)))
                                    (set-Name-gdef! b glob)
                                    (set! newenv (extend-env newenv x b))
                                    (make-Define b box-e))))
                             (else
                              (when (bound? newenv x)
                                (syntax-error #f "~a defined more than once" x))
                              (let ((b (bind-var x)))
                                (set! newenv (extend-env newenv x b))
                                (make-Define b box-e)))))
                      ((and d ($ Defstruct tag args make pred get set mutable))
                       (let* ((make (struct-def make (andmap not mutable)))
                              (pred (struct-def pred #t))
                              (bind-get
                               (lambda (name n)
                                 (match name
                                   (($ Some x)
                                    (let ((b (struct-def x #t)))
                                      (set-Name-selector! b
                                        (lambda (X)
                                          (make-Pobj
                                            pred
                                            (map-with-n (lambda (_ m)
                                                          (if (= m n)
                                                              X
                                                              (make-Pany)))
                                              get))))
                                      (Some b)))
                                   (none none))))
                              (bind-set
                               (match-lambda
                                 (($ Some x) (Some (struct-def x #t)))
                                 (none none)))
                              (get (map-with-n bind-get get))
                              (set (map bind-set set))
                              (_ (when (bound? newtenv tag)
                                   (syntax-error (pdef d)
                                     "type constructor ~a defined more than once" tag)))
                              (tc (bind-tycon
                                    tag
                                    mutable
                                    (bound? tenv tag)
                                    (lambda args
                                      (apply syntax-error (cons (pdef d) args))))))
                         (set! newtenv (extend-env newtenv tag tc))
                         (set-Name-predicate! pred
                           `(,tc ,@(map (lambda (_) (gensym)) get)))
                         (make-Defstruct tc args make pred get set mutable)))
                      ((and d ($ Datatype dt))
                       (make-Datatype
                         (mapLR
                           (match-lambda
                             (((tag . args) . bindings)
                              (when (bound? newtenv tag)
                                (syntax-error (pdef d)
                                  "type constructor ~a defined more than once" tag))
                              (let ((tc (bind-tycon
                                          tag
                                          (map (lambda (_) #f) args)
                                          (bound? tenv tag)
                                          (lambda args
                                            (apply syntax-error (cons (pdef d) args))))))
                                (set! newtenv (extend-env newtenv tag tc))
                                (cons (cons tc args)
                                      (mapLR
                                        (match-lambda
                                          (($ Variant con pred arg-types)
                                           (let ((make (struct-def con #t))
                                                 (pred (struct-def pred #t)))
                                             (set-Name-predicate! pred (cons tc args))
                                             (set-Name-variant! pred arg-types)
                                             (make-Variant make pred arg-types))))
                                        bindings)))))
                           dt)))))
             (defs2 (mapLR bind1 defs))
             (newenv2 (join-env env newenv))
             (newtenv2 (join-env tenv newtenv))
             (bind2 (match-lambda
                      ((and ($ Define x box-e2) context)
                       (make-Define
                         (or x (let ((b (bind-var x)))
                                 (set-Name-gdef! b glob)
                                 b))
                         (box (bind (unbox box-e2) newenv2 newtenv2 context))))
                      (d d))))
            (list (mapLR bind2 defs2) newenv2 newtenv2))))
       (use-var
        (lambda (env x context)
          (let ((b (lookup-or-fail env x
                     (lambda ()
                       (lookup-or-fail unbound-env x
                         (lambda ()
                           (set! n-unbound (+ 1 n-unbound))
                           (printf "Warning: name ~a is unbound in " x)
                           (print-context (pexpr context) 2)
                           (let ((b (bind-var x)))
                             (set! unbound-env (extend-env unbound-env x b))
                             b)))))))
            (when (and (Name-primitive b) (memq x cons-mutators))
              (set! cons-is-mutable #t))
            (set-Name-occ! b (+ 1 (Name-occ b)))
            b))))
      (match-let* (((tree env tenv)
                    (bind-defn defs initial-env initial-type-env #t))
                   (prim (map (lambda (b)
                                (let ((p (rebind-var b)))
                                  (set-Name-mutated!   p #f)
                                  (set-Name-primitive! p (Name-primitive b))
                                  (set-Name-struct!    p (Name-struct b))
                                  (set-Name-pure!      p (Name-pure b))
                                  (set-Name-predicate! p (Name-predicate b))
                                  (set-Name-variant!   p (Name-variant b))
                                  (set-Name-selector!  p (Name-selector b))
                                  (set-Name-subtyped!  p (Name-subtyped b))
                                  (set-Name-ty!        b #f)
                                  (set-Name-gdef!      b #t)
                                  (set-Name-primitive! b #f)
                                  (set-Name-struct!    b #f)
                                  (set-Name-pure!      b #f)
                                  (set-Name-predicate! b #f)
                                  (set-Name-variant!   b #f)
                                  (set-Name-selector!  b #f)
                                  (set-Name-subtyped!  b #f)
                                  (make-Define b (box (make-Var p)))))
                              redefined-prims)))
        (set-cons-mutability! cons-is-mutable)
        (unless cons-is-mutable
          (printf "Note: no use of ~a, treating cons as immutable~%"
            cons-mutators))
        (unless (null? redefined-prims)
          (for-each
            (lambda (b)
              (printf "Note: primitive ~a redefined~%" (pname b)))
            redefined-prims)
          (set! init-env-mangled #t))
        (list (append prim tree) env tenv)))))

(define bind-var
  (lambda (x)
    (make-Name x #f 0 #f #f #f #f #f #f #f #f #f)))

(define rebind-var
  (lambda (b)
    (make-Name
      (Name-name b)
      (Name-ty b)
      (Name-occ b) ; inaccurate
      (Name-mutated b)
      #f
      #f
      #f
      #f
      #f
      #f
      #f
      #f)))
