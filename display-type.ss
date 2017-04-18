;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for displaying types of things

(define display-type tidy)

(define type
  (lambda names
    (if (null? names)
        (for-each globaldef tree)
        (for-each
          (match-lambda
            ((? symbol? x)
             (let* ((ty (lookup-or-fail global-env x
                          (lambda ()
                            (use-error "~a is not defined" x))))
                    (type (display-type (Name-ty ty))))
               (pretty-print `(,x : ,type))))
            ((? number? n)
             (let* ((ty (check-type (tree-unindex n)))
                    (type (display-type ty)))
               (pretty-print `(,n : ,type))))
            (_
             (use-error "arguments must be indentifiers or CHECK numbers")))
          names))))

(define localtype
  (lambda names
    (if (null? names)
        (for-each localdef tree)
        (for-each (lambda (x) (localdef (find-global x))) names))))

(define find-global
  (lambda (name)
    (let ((d (ormap (match-lambda
                      ((and d ($ Define x _)) (and (eq? name (Name-name x)) d))
                      (_ #f))
               tree)))
      (unless d (use-error "~a is not defined" name))
      d)))                

(define globaldef
  (lambda (e)
    (match e
      (($ Define x ($ box e))
       (let ((type (display-type (Name-ty x))))
         (pretty-print `(,(pname x) : ,type))))
      (_ #f))))

(define localdef
  (lambda (e)
    (pretty-print (expdef e))))

(define expdef
  (let* ((show (lambda (x) `(,(pname x) : ,(display-type (Name-ty x)))))
         (pbind (match-lambda
                  (($ Bind x e) `(,(show x) ,(expdef e))))))
    (match-lambda
      (($ Define x ($ box e))
       (if (or (not x) (and (Name? x) (not (Name-name x))))
           (expdef e)
           `(define ,(show x) ,(expdef e))))
      ((? Defstruct? d)
       (pdef d))
      ((? Datatype? d)
       (pdef d))
      (($ And exps)
       `(and ,@(mapLR expdef exps)))
      (($ App fun args)
       `(,(expdef fun) ,@(mapLR expdef args)))
      (($ Begin exps)
       `(begin ,@(mapLR expdef exps)))
      (($ Const c _)
       (pconst c))
      (($ If test then els)
       `(if ,(expdef test) ,(expdef then) ,(expdef els)))
      (($ Lam params body)
       `(lambda ,(map show params) ,@(expdef body)))
      (($ Vlam params rest body)
       `(lambda ,(append (map show params) (show rest)) ,@(expdef body)))
      (($ Let bindings body)
       `(let ,(map pbind bindings) ,@(expdef body)))
      (($ Let* bindings body)
       `(let* ,(map pbind bindings) ,@(expdef body)))
      (($ Letr bindings body)
       `(letrec ,(map pbind bindings) ,@(expdef body)))
      (($ Body defs exps)
       (let ((pdefs (mapLR expdef defs)))
         (append pdefs (mapLR expdef exps))))
      (($ Record bindings)
       `(record ,@(mapLR pbind bindings)))
      (($ Field x e)
       `(field ,x ,(expdef e)))
      (($ Cast (ty . _) e)
       `(: ,ty ,(expdef e)))
      (($ Or exps)
       `(or ,@(mapLR expdef exps)))
      (($ Delay e)
       `(delay ,(expdef e)))
      (($ Set! x body)
       `(set! ,(pname x) ,(expdef body)))
      (($ Var x)
       (pname x))
      (($ Match e1 clauses)
       (let* ((pclause
               (match-lambda
                 (($ Mclause p exp fail)
                  (if fail
                      `(,(expdef p) (=> ,(pname fail)) ,@(expdef exp))
                      `(,(expdef p) ,@(expdef exp))))))
              (p1 (expdef e1)))
         `(match ,p1 ,@(mapLR pclause clauses))))
      (($ Pconst x _)
       (pconst x))
      (($ Pvar x)
       (show x))
      (($ Pany)
       '_)
      (($ Pelse)
       'else)
      (($ Pnot pat)
       `(not ,(expdef pat)))
      (($ Pand pats)
       `(and ,@(mapLR expdef pats)))
      (($ Ppred pred)
       (match (pname pred)
         ('false-object? #f)
         ('true-object? #t)
         ('null? ())
         (x `(? ,x))))
      (($ Pobj tag args)
       (match (cons (pname tag) args)
         (('pair? x y) (cons (expdef x) (expdef y)))
         (('box? x) (box (expdef x)))
         (('vector? . x) (list->vector (mapLR expdef x)))
         ((tg . _) `($ ,(strip-? tg) ,@(mapLR expdef args)))))
      (($ Type _ exp)
       (expdef exp))
      (($ Check _ exp)
       (expdef exp)))))

(define check-type
  (match-lambda
    (($ Type ty ($ Check inf ($ Var x)))
     ty)
    (($ Type ty ($ Check inf ($ Lam x e1)))
     ty)
    (($ Type ty ($ Check inf ($ Vlam x rest e1)))
     ty)
    (($ Type _ ($ Check inf ($ App e1 args)))
     (typeof e1))
    (($ Type _ ($ Check inf ($ Field x e1)))
     (typeof e1))
    (($ Type _ ($ Check inf ($ Cast (x . _) e1)))
     (typeof e1))
    (($ Type _ ($ Check inf ($ Match e1 clauses)))
     (typeof e1))))
