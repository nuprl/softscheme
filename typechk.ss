;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Inference

(define GenLet #t)
(define GenMatch #t)

(define type-defs
  (lambda (d)
    (for-each
      (match-lambda
        ((? Defstruct? b) (type-structure b))
        ((? Datatype? b) (type-structure b))
        (c (type-component c ord-depth #t)))
      (make-components d))
    (for-each
      (match-lambda
        (($ Define b _)
         (unless (TS? (Name-ty b))
           (visible! (Name-ty b) ord-depth)))
        (_ #f))
      d)))
      
(define type-structure
  (match-lambda
    (($ Defstruct x _ make pred get set mutable)
     (let ((vars (map (lambda (_) (gensym)) get)))
       (set-Name-ty! make
         (closeall (R+ ord-depth initial-type-env
                       `(,@vars -> (,x ,@vars)))))
       (set-Name-ty! pred
         (closeall (R+ ord-depth initial-type-env
                       `((+ (,x ,@vars) y) -> bool))))
       (for-each2
         (lambda (getter v)
           (match getter
             (($ Some b)
              (set-Name-ty! b
                (closeall (R+ ord-depth initial-type-env
                              `((,x ,@vars) -> ,v)))))
             (_ #f)))
         get vars)
       (for-each2
         (lambda (setter v)
           (match setter
             (($ Some b)
              (set-Name-ty! b
                (closeall (R+ ord-depth initial-type-env
                              `((,x ,@vars) ,v -> void)))))
             (_ #f)))
         set vars)))
    (($ Datatype dt)
     (for-each
       (match-lambda
         ((type . variants)
          (for-each
            (match-lambda
              (($ Variant con pred arg-types)
               (set-Name-ty! con
                 (closeall (R+ ord-depth initial-type-env
                               `(,@(cdr arg-types) -> ,type))))
               (set-Name-ty! pred
                 (closeall (R+ ord-depth initial-type-env
                               `((+ ,(Name-predicate pred) x) -> bool))))))
            variants)))
       dt))))

(define type-component
  (lambda (component depth top)
    (when verbose
      (let ((cnames (filter-map
                      (match-lambda (($ Define b _) (Name-name b)))
                      component)))
        (unless (null? cnames)
          (printf "Typing ~a~%" cnames))))
    (let* ((f (match-lambda (($ Define b ($ box e)) (make-Bind b e))))
           (bindings (map f component))
           (gen (generalizable? bindings))
           (depth (if gen (+ 1 depth) depth))
           (names (map (match-lambda (($ Define b ($ box e)) (pname b))) component))
           (f1 (match-lambda
                 (($ Define b _)
                  (set-Name-ty! b (Tvar depth)))))
           (f2 (match-lambda
                 (($ Define b box-e)
                  (set-box! box-e (W (unbox box-e) depth names)))))
           (f3 (match-lambda
                 (($ Define b ($ box e))
                  (unify (Name-ty b) (typeof e)))))
           (f4 (match-lambda
                 (($ Define b ($ box e))
                  (set-Name-ty! b (close (Name-ty b) depth))))))
      (for-each f1 component)
      (for-each f2 component)
      (for-each f3 component)
      (cond
        (gen
         (new-close-depth)
         (for-each f4 component))
        (top
         (let ((cnames (filter-map
                         (match-lambda (($ Define b _) (Name-name b)))
                         component)))
           (unless (null? cnames)
             (printf "Note: not generalizing ~a~%" cnames))))))))

(define W
  (lambda (e depth component)
    (match e
      (($ Const _ pred)
       (make-Type (R+ depth initial-type-env (Name-predicate pred)) e))
      (($ Var x)
       (unless (Name-ty x)
         ; set up type for unbound references
         (set-Name-ty! x
           (if (Name-mutated x)
               (Tvar ord-depth)
               (closeall (Tvar ord-depth)))))
       (if (TS? (Name-ty x))
           (match-let* ((tynode (make-Type #f #f))
                        ((t absv) (instantiate (Name-ty x) depth tynode)))
             (set-Type-ty! tynode t)
             (set-Type-exp! tynode
               (match (Name-primitive x)
                 ('imprecise
                   (make-Check (list absv #f #f #f component) e))
                 ('check
                   (make-Check (list (cons #t absv) #f #f #f component) e))
                 ('nocheck
                   e)
                 (#t
                   (make-Check (list absv (mk-definite-prim t) #f #f component) e))
                 (#f
                   (make-Check (list absv #f #f #t component) e))))
             tynode)
           e))
      (($ Lam x e1)
       (for-each (lambda (b) (set-Name-ty! b (Tvar depth))) x)
       (match-let* ((body (W e1 depth component))
                    ((t absv)
                     (R+collect depth initial-type-env
                       `(,@(map Name-ty x) -> ,(typeof body)))))
         (make-Type t
           (make-Check (list absv (mk-definite-lam t) #f #f component)
             (make-Lam x body)))))
      (($ Vlam x rest e1)
       (for-each (lambda (b) (set-Name-ty! b (Tvar depth))) x)
       (let ((z (Tvar depth)))
         (set-Name-ty! rest (R+ depth initial-type-env `(list ,z)))
         (letrec ((loop (match-lambda
                          (() `(arglist ,z))
                          ((a . b) `(arg ,(Name-ty a) ,(loop b))))))
           (match-let* ((body (W e1 depth component))
                        ((t absv)
                         (R+collect depth initial-type-env
                           `(,(loop x) ->* ,(typeof body)))))
             (make-Type t
               (make-Check (list absv (mk-definite-lam t) #f #f component)
                 (make-Vlam x rest body)))))))
      (($ App e0 args)
       (match-let* ((t0 (W e0 depth component))
                    (targs (mapLR (lambda (e) (W e depth component)) args))
                    (a* (map (lambda (_) (Tvar depth)) args))
                    (b (Tvar depth))
                    ((t absv)
                     (R-collect depth initial-type-env `(,@a* -> ,b)))
                    (definf (mk-definite-app t)))
         (unify (typeof t0) t)
         (for-each2 unify (map typeof targs) a*)
         (if (syntactically-a-procedure? t0)
             (make-Type b
               (make-App t0 targs))
             (make-Type b
               (make-Check (list absv definf #f #f component)
                 (make-App t0 targs))))))
      (($ Let b e2)
       (let* ((do-bind
               (match-lambda
                 ((and bind ($ Bind b e))
                  (if (and GenLet (generalizable? (list bind)))
                      (let* ((names (list (pname b)))
                             (e (W e (+ 1 depth) names)))
                        (new-close-depth)
                        (set-Name-ty! b (close (typeof e) (+ 1 depth)))
                        (make-Bind b e))
                      (let ((e (W e depth component)))
                        (set-Name-ty! b (typeof e))
                        (make-Bind b e))))))
              (tb (map do-bind b))
              (body (W e2 depth component)))
         (make-Let tb body)))
      (($ Let* b e2)
       (let* ((do-bind
               (match-lambda
                 ((and bind ($ Bind b e))
                  (if (and GenLet (generalizable? (list bind)))
                      (let* ((names (list (pname b)))
                             (e (W e (+ 1 depth) names)))
                        (new-close-depth)
                        (set-Name-ty! b (close (typeof e) (+ 1 depth)))
                        (make-Bind b e))
                      (let ((e (W e depth component)))
                        (set-Name-ty! b (typeof e))
                        (make-Bind b e))))))
              (tb (mapLR do-bind b))
              (body (W e2 depth component)))
         (make-Let* tb body)))
      (($ Letr b e2)
       ;; this code reorders letrecs, which is ok by R4RS
       (let* ((do-comp
               (lambda (b)
                 (let* ((gen (and GenLet (generalizable? b)))
                        (depth (if gen (+ 1 depth) depth))
                        (names (if gen
                                   (map (match-lambda (($ Bind b _) (pname b)))
                                        b)
                                   component))
                        (_ (for-each
                             (match-lambda
                               (($ Bind b _) (set-Name-ty! b (Tvar depth))))
                             b))
                        (tb (mapLR
                              (match-lambda
                                (($ Bind b e) (make-Bind b (W e depth names))))
                              b)))
                   (for-each
                     (match-lambda (($ Bind b e)
                                    (unify (Name-ty b) (typeof e))))
                     tb)
                   (when gen
                     (new-close-depth)
                     (for-each
                       (match-lambda
                         (($ Bind b _)
                          (set-Name-ty! b (close (Name-ty b) depth))))
                       tb))
                   tb)))
              (comps (make-letrec-components b))
              (tb (foldr append () (mapLR do-comp comps))))
         (make-Letr tb (W e2 depth component))))
      (($ Body defs exps)
       (for-each
         (match-lambda
           ((? Defstruct? b) (type-structure b))
           ((? Datatype? b) (type-structure b))
           (c (type-component c depth #f)))
         (make-body-components defs))
       (let ((texps (mapLR (lambda (x) (W x depth component)) exps)))
         (make-Body defs texps)))
      (($ And exps)
       (let* ((texps (mapLR (lambda (x) (W x depth component)) exps))
              (t (match texps
                   (() (R+ depth initial-type-env 'true))
                   ((e) (typeof e))
                   (_ (let ((a (R+ depth initial-type-env 'false)))
                        (unify (typeof (rac texps)) a)
                        a)))))
         (make-Type t
           (make-And texps))))
      (($ Or exps)
       (let* ((texps (mapLR (lambda (x) (W x depth component)) exps))
              (t (match texps
                   (() (R+ depth initial-type-env 'false))
                   ((e) (typeof e))
                   (_ (let* ((t-last (typeof (rac texps)))
                             (but-last (rdc texps))
                             (a (Tvar depth)))
                        (for-each
                          (lambda (e)
                            (unify
                              (typeof e)
                              (R+ depth initial-type-env `(+ (not false) ,a))))
                          but-last)
                        (unify
                          t-last
                          (R+ depth initial-type-env `(+ (not false) ,a)))
                        t-last)))))
         (make-Type t
           (make-Or texps))))
      (($ Begin exps)
       (let ((texps (mapLR (lambda (x) (W x depth component)) exps)))
         (make-Begin texps)))
      (($ If test then els)
       (let ((ttest (W test depth component))
             (tthen (W then depth component))
             (tels (W els depth component))
             (a (Tvar depth)))
         (unify (typeof tthen) a)
         (unify (typeof tels) a)
         (make-Type a
           (make-If ttest tthen tels))))
      (($ Delay e2)
       (let ((texp (W e2 depth component)))
         (make-Type (R+ depth initial-type-env `(promise ,(typeof texp)))
           (make-Delay texp))))
      (($ Set! x body)
       (unless (Name-ty x)
           ; set up type for unbound references
         (set-Name-ty! x
           (if (Name-mutated x)
               (Tvar ord-depth)
               (closeall (Tvar ord-depth)))))
       (let ((body (W body depth component)))
         (unify (Name-ty x) (typeof body))
         (make-Type (R+ depth initial-type-env 'void)
           (make-Set! x body))))
      (($ Record bind)
       (let* ((tbind (map (match-lambda
                            (($ Bind name exp)
                             (make-Bind name (W exp depth component))))
                          bind))
              (t (R+ depth initial-type-env
                     `(record ,@(map (match-lambda
                                       (($ Bind name exp)
                                        (list name (typeof exp))))
                                     tbind)))))
         (make-Type t
           (make-Record tbind))))
      (($ Field name exp)
       (match-let* ((texp (W exp depth component))
                    (a (Tvar depth))
                    ((t absv) (R-collect depth initial-type-env
                                `(record (,name ,a)))))
         (unify (typeof texp) t)
         (make-Type a
           (make-Check (list absv #f #f #f component)
             (make-Field name texp)))))
      (($ Cast (ty . tenv) exp)
       (match-let* ((texp (W exp depth component))
                    ((t absv)
                     (handle
                       (R+collect depth tenv
                         (match ty
                           (('rec bind ty2)
                            `(rec ,bind (,ty2 -> ,ty2)))
                           (_
                            `(,ty -> ,ty))))
                       (match-lambda*
                         (('type . args) (apply syntax-error ty args))
                         (x (apply raise x)))))
                    (a (Tvar depth)))
         (unify (R+ depth initial-type-env `(,(typeof texp) -> ,a)) t)
         (make-Type a
           (make-Check (list absv #f #f #f component)
             (make-Cast (cons ty tenv) texp)))))
      (($ Match exp clauses)
       (for-each
         (match-lambda
           (($ Mclause p _ (? Name? fail))
            (set-Name-ty! fail (R+ depth initial-type-env '(a ->* b))))
           (_ #f))
         clauses)
       (match-let* ((iclauses (improve-clauses
                                (append clauses
                                  (list (make-Mclause (make-Pelse) #f #f)))))
                    ((tmatch absv bindings precise)
                     (W-match (rdc iclauses) (rac iclauses) depth))
                    (_ (for-each
                         (match-lambda
                           ((b . t)
                            (if (and GenMatch
                                     (not (Name-mutated b))
                                     (< 1 (Name-occ b)))
                                (begin
                                  (new-close-depth)
                                  (set-Name-ty! b (close t (+ 1 depth))))
                                (begin
                                  (set-Name-ty! b t)))))
                         bindings))
                    (texp (W exp depth component))
                    (_ (unify (typeof texp) tmatch))
                    (tclauses (mapLR
                                (match-lambda
                                  (($ Mclause p e fail)
                                   (make-Mclause p (W e depth component) fail)))
                                clauses))
                    (a (Tvar depth)))
         (for-each
           (match-lambda (($ Mclause _ e _) (unify (typeof e) a)))
           tclauses)
         (make-Type a
           (make-Check (list absv #f (not precise) #f component)
             (make-Match texp tclauses))))))))

(define W-match
  (lambda (clauses last depth)
    (letrec
      ((bindings ())
       (loop
        (match-lambda
          (($ Pconst _ pred)
           (R+pat depth (Name-predicate pred)))
          (($ Pvar x)
           (let ((t (PTvar depth)))
             (set! bindings (cons (cons x t) bindings))
             t))
          (($ Pany)
           (PTvar depth))
          (($ Pelse)
           (Tvar depth))
          (($ Pand pats)
           (let* ((vars (filter Pvar? pats))
                  (non-vars (filter (lambda (x) (not (Pvar? x))) pats))
                  (tys (mapLR loop non-vars))
                  (t (if (null? tys)
                         (PTvar depth)
                         (foldr (lambda (x y) (and-ty x y) y)
                           (car tys)
                           (cdr tys)))))
             (for-each
               (match-lambda
                 (($ Pvar x)
                  (let* ((gdepth (if (and GenMatch
                                          (not (Name-mutated x))
                                          (< 1 (Name-occ x)))
                                     (+ 1 depth)
                                     depth))
                         (vtype (pat-var-bind t gdepth)))
                    (set! bindings (cons (cons x vtype) bindings)))))
               vars)
             t))
          (($ Pnot pat)
           (notloop pat))
          (($ Ppred pred)
           (match (Name-name pred)
             ('boolean? (R+pat depth 'bool))
             ('list? (R+pat depth '(list _)))
             (_ (R+pat depth (Name-predicate pred)))))
          ((and pat ($ Pobj c args))
           (cond
             ((memq (Name-name c) '(vector? cvector?))
              (R+pat depth
                (cons (case (Name-name c)
                        ((vector?) 'vec)
                        ((cvector?) 'cvec))
                      (match (mapLR loop args)
                        (() (list (PTvar depth)))
                        ((first . rest)
                         (list (foldr (lambda (x y) (or-ty x y) y)
                                 first
                                 rest)))))))
             ((Name-variant c)
              (unless (= (1- (length (Name-variant c))) (length args))
                (syntax-error (ppat pat)
                  "~a requires ~a sub-patterns"
                  (strip-? (Name-name c))
                  (1- (length (Name-variant c)))))
              (letrec ((sequence
                        (match-lambda
                          (() 'noarg)
                          ((x . y) `(arg ,x ,(sequence y))))))
                (let* ((x (R+pat depth
                            (sequence
                              (cons (Name-predicate c)
                                    (cdr (Name-variant c))))))
                       (y (Tvar depth))
                       (z (R+pat depth
                            (sequence
                              (cons y (mapLR loop args))))))
                  (unify x z)
                  y)))
             (else
              (unless (= (length (cdr (Name-predicate c))) (length args))
                (syntax-error (ppat pat)
                  "~a requires ~a sub-patterns"
                  (car (Name-predicate c))
                  (length (cdr (Name-predicate c)))))
              (R+pat depth
                (cons (car (Name-predicate c))
                      (mapLR loop args))))))))
       (notloop
        (match-lambda
          ((? Pconst?)
           (PTvar depth))
          ((? Pvar?)
           (Tvar depth))
          ((? Pany?)
           (Tvar depth))
          ((? Pelse?)
           (PTvar depth))
          (($ Pand (x . _))
           (notloop x))
          (($ Ppred pred)
           (case (Name-name pred)
             ((boolean?) (R+pat depth `(+ (not true) (not false) _)))
             ((procedure?) (R+pat depth `(+ (not ->*) _)))
             ((list?) (PTvar depth))
             (else (R+pat depth `(+ (not ,(car (Name-predicate pred))) _)))))
          (($ Pobj pred pats)
           (let ((m (foldr + 0 (map non-triv pats))))
             (case m
               ((0) 
                (R+pat depth `(+ (not ,(car (Name-predicate pred))) _)))
               ((1)
                (R+pat depth `(+ (,(car (Name-predicate pred))
                                  ,@(map (lambda (x)
                                           (if (zero? (non-triv x))
                                               '_
                                               (notloop x)))
                                         pats))
                                 _)))
               (else
                (PTvar depth)))))))
       (non-triv
        (match-lambda
          ((? Pvar?) 0)
          ((? Pany?) 0)
          ((? Pelse?) 0)
          (($ Pand (x . _)) (non-triv x))
          (_ 1)))
       (precise
        (match-lambda
          ((? Pconst?) #f)
          (($ Pand pats) (andmap precise pats))
          (($ Pnot pat) (precise pat))
          (($ Pobj pred pats)
           (let ((m (foldr + 0 (map non-triv pats))))
             (case m
               ((0) #t)
               ((1) (andmap precise pats))
               (else #f))))
          (($ Ppred pred)
           (case (Name-name pred)
             ((list?) #f)
             (else #t)))
          (_ #t))))
      (let* ((types
              (mapLR
                (match-lambda (($ Mclause p _ _) (loop p)))
                clauses))
             (precise-match
              (and (andmap
                     (match-lambda (($ Mclause _ _ fail) (not fail)))
                     clauses)
                   (match last
                     (($ Mclause p _ _) (precise p))))))
        `(,@(R-match types bindings depth)
             ,bindings
             ,precise-match)))))

(define syntactically-a-procedure?
  (match-lambda
    (($ Type _ e) (syntactically-a-procedure? e))
    (($ Check _ e) (syntactically-a-procedure? e))
    (($ Var x) (Name-primitive x))
    ((? Lam?) #t)
    ((? Vlam?) #t)
    (($ Let _ body)  (syntactically-a-procedure? body))
    (($ Let* _ body) (syntactically-a-procedure? body))
    (($ Letr _ body) (syntactically-a-procedure? body))
    (($ If _ e2 e3) (and (syntactically-a-procedure? e2)
                         (syntactically-a-procedure? e3)))
    (($ Begin exps) (syntactically-a-procedure? (rac exps)))
    (($ Body _ exps) (syntactically-a-procedure? (rac exps)))
    (_ #f)))
      
(define typeof
  (match-lambda
    (($ Type t _) t)
    (($ Check _ e) (typeof e))
    (($ Let _ body) (typeof body))
    (($ Let* _ body) (typeof body))
    (($ Letr _ body) (typeof body))
    (($ Body _ exps) (typeof (rac exps)))
    (($ Begin exps) (typeof (rac exps)))
    (($ Var x) (Name-ty x))))

(define generalizable?
  (lambda (bindings)
    (and (andmap
           (match-lambda
             (($ Bind x e)
              (and (not (Name-mutated x)) (not (expansive? e)))))
           bindings)
         (ormap
           (match-lambda
             (($ Bind x _)
              (or (Name-gdef x) (< 1 (Name-occ x)))))
           bindings))))

(define expansive?
  (letrec ((expansive-binding?
            (match-lambda
              (($ Bind b e) (or (Name-mutated b) (expansive? e)))))
           (expansive-pattern?
            (match-lambda
              ((? Pconst?) #f)
              (($ Pvar x) (Name-mutated x))
              (($ Pobj _ pats) (ormap expansive-pattern? pats))
              ((? Pany?) #f)
              ((? Pelse?) #f)
              (($ Pand pats) (ormap expansive-pattern? pats))
              (($ Ppred x) (Name-mutated x))
              (($ Pnot pat) (expansive-pattern? pat)))))
    (match-lambda
      ((? Defstruct?) #f)
      ((? Datatype?) #f)
      (($ Define x ($ box e)) (or (Name-mutated x) (expansive? e)))
      (($ App ($ Var x) exps)
       (or (not (Name-pure x))
           (and (eq? 'cons (Name-pure x)) cons-is-mutable)
           (ormap expansive? exps)))
      (($ App _ _) #t)
      (($ Delay _) #f)
      (($ Set! _ _) #t)
      (($ Var _) #f)
      ((? Const?) #f)
      (($ Lam _ _) #f)
      (($ Vlam _ _ _) #f)
      (($ Let bind body)
       (or (expansive? body)
           (ormap expansive-binding? bind)))
      (($ Let* bind body)
       (or (expansive? body)
           (ormap expansive-binding? bind)))
      (($ Letr bind body)
       (or (expansive? body)
           (ormap expansive-binding? bind)))
      (($ Body defs exps)
       (or (ormap expansive? defs) (ormap expansive? exps)))
      (($ And exps) (ormap expansive? exps))
      (($ Or exps) (ormap expansive? exps))
      (($ Begin exps) (ormap expansive? exps))
      (($ If e1 e2 e3)
       (or (expansive? e1) (expansive? e2) (expansive? e3)))
      (($ Record bind)
       (ormap (match-lambda (($ Bind x e) (expansive? e))) bind))
      (($ Field _ exp)
       (expansive? exp))
      (($ Cast _ exp)
       (expansive? exp))
      (($ Match exp clauses)
       (or (expansive? exp)
           (ormap
             (match-lambda (($ Mclause pat body fail)
                            (or (and fail (Name-mutated fail))
                                (expansive-pattern? pat)
                                (expansive? body))))
             clauses)))
      (($ Type _ e1) (expansive? e1))
      (($ Check _ e1) (expansive? e1)))))
