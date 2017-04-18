;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unparse

(define unparse
  (lambda (e check-action)
    (letrec
      ((pbind
        (match-lambda
          (($ Bind n e) (list (pname n) (pexpr e)))))
       (pexpr
        (match-lambda
          ((and x ($ Type _ (? Check?)))
           (check-action x pexpr))
          (($ Type _ exp)
           (pexpr exp))
          (($ Shape t exp)
;           `(SHAPE ,(display-type t) ,(pexpr exp))
           (pexpr exp))
          (($ Define x ($ box e))
           (if (or (not x) (and (Name? x) (not (Name-name x))))
               (pexpr e)
               `(define ,(pname x) ,(pexpr e))))
          (($ Defstruct _ args _ _ _ _ _)
           `(CHECK-define-const-structure ,args))
          (($ Datatype d)
           `(datatype
              ,@(map
                  (match-lambda
                    (((tag . args) . bindings)
                     (cons (cons (ptag tag) args)
                           (map
                             (match-lambda
                               (($ Variant _ _ types) types))
                             bindings))))
                  d)))
          (($ And exps)
           `(and ,@(mapLR pexpr exps)))
          (($ Or exps)
           `(or ,@(mapLR pexpr exps)))
          (($ Begin exps)
           `(begin ,@(mapLR pexpr exps)))
          (($ Var x)
           (pname x))
          (($ Prim x)
           (pname x))
          (($ Const x _)
           (pconst x))
          (($ Lam x e1)
           `(lambda ,(mapLR pname x) ,@(pexpr e1)))
          (($ Vlam x rest e1)
           `(lambda ,(append (mapLR pname x) (pname rest))
              ,@(pexpr e1)))
          (($ Match e1 clauses)
           (let* ((pclause
                   (match-lambda
                     (($ Mclause p exp fail)
                      (if fail
                          `(,(ppat p) (=> ,(pname fail)) ,@(pexpr exp))
                          `(,(ppat p) ,@(pexpr exp))))))
                  (p1 (pexpr e1)))
             `(match ,p1 ,@(mapLR pclause clauses))))
          (($ App e1 args)
;           `(,(pexpr e1) ,@(mapLR pexpr args))
           (let* ((p1 (pexpr e1))
                  (pargs (mapLR pexpr args))
                  (unkwote (match-lambda
                             (('quote x) x)
                             ((? boolean? x) x)
                             ((? number? x) x)
                             ((? char? x) x)
                             ((? string? x) x)
                             ((? null? x) x)
                             ((? box? x) x)
                             ((? vector? x) x))))
             (cond ((eq? p1 Qlist)
                    `(quote ,(mapLR unkwote pargs)))
                   ((eq? p1 Qcons)
                    (let ((unq (mapLR unkwote pargs)))
                      `(quote ,(cons (car unq) (cadr unq)))))
                   ((eq? p1 Qbox)
                    (box (unkwote (car pargs))))
                   ((eq? p1 Qvector)
                    (list->vector (mapLR unkwote pargs)))
                   (else (cons p1 pargs)))))
          (($ Let b e2)
           (let ((pb (mapLR pbind b)))
             `(let ,pb ,@(pexpr e2))))
          (($ Let* b e2)
           (let ((pb (mapLR pbind b)))
             `(let* ,pb ,@(pexpr e2))))
          (($ Letr b e2)
           (let ((pb (mapLR pbind b)))
             `(letrec ,pb ,@(pexpr e2))))
          (($ Body defs exps)
           (let ((pdefs (mapLR pexpr defs)))
             (append pdefs (mapLR pexpr exps))))
          (($ If e1 e2 e3)
           (let* ((p1 (pexpr e1))
                  (p2 (pexpr e2))
                  (p3 (pexpr e3)))
             `(if ,p1 ,p2 ,p3)))
          (($ Record bindings)
           `(record ,@(mapLR pbind bindings)))
          (($ Field x e2)
           `(field ,x ,(pexpr e2)))
          (($ Cast (ty . _) e2)
           `(: ,ty ,(pexpr e2)))
;          (($ Module names defs)
;           `(module ,(mapLR pname names) ,@(mapLR pexpr defs)))
;          (($ Import iclauses body)
;           (let* ((pbinding (match-lambda
;                              (($ Bind i e)
;                               (if (eq? i e)
;                                   (pname i)
;                                   (list (pname i) (pname e))))))
;                  (pimport (match-lambda
;                             (($ Iclause exp bindings)
;                              (cons (pexpr exp) (mapLR pbinding bindings))))))
;             `(import ,(mapLR pimport iclauses) ,@(pexpr body))))
          (($ Delay e)
           `(delay ,(pexpr e)))
          (($ Set! x e)
           `(set! ,(pname x) ,(pexpr e))))))
      (pexpr e))))

(define pexpr
  (lambda (ex)
    (unparse
      ex
      (lambda (e pexpr)
        (match e
          (($ Type _ ($ Check _ exp)) (pexpr exp)))))))

(define pdef pexpr)

(define ppat
  (match-lambda
    (($ Pconst x _)
     (pconst x))
    (($ Pvar x)
     (pname x))
    (($ Pany)
     '_)
    (($ Pelse)
     'else)
    (($ Pnot pat)
     `(not ,(ppat pat)))
    (($ Pand pats)
     `(and ,@(mapLR ppat pats)))
    (($ Ppred pred)
     (match (pname pred)
       ('false-object? #f)
       ('true-object? #t)
       ('null? ())
       (x `(? ,x))))
    (($ Pobj tag args)
     (match (cons (pname tag) args)
       (('box? x) (box (ppat x)))
       (('pair? x y) (cons (ppat x) (ppat y)))
       (('vector? . x) (list->vector (mapLR ppat x)))
       ((tg . _) `($ ,(strip-? tg) ,@(mapLR ppat args)))))))

(define strip-?
  (lambda (s)
    (let* ((str (symbol->string s))
           (n (string-length str)))
      (if (or (zero? n) (not (char=? #\? (string-ref str (sub1 n)))))
          s
          (string->symbol (substring str 0 (sub1 n)))))))

(define pname
  (match-lambda
    ((? Name? x) (or (Name-name x) '<expr>))
    ((? symbol? x) x)))

(define ptag
  (match-lambda
    ((? K? k) (K-name k))
    ((? symbol? x) x)))

(define pconst
  (match-lambda
    ((? symbol? x) `(quote ,x))
    ((? boolean? x) x)
    ((? number? x) x)
    ((? char? x) x)
    ((? string? x) x)
    ((? null? x) x)))
