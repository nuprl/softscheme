;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abstract Syntax definitions.

; option =
(define-const-structure (Some _))
(define-const-structure (None))
(define None (make-None))
(define Some make-Some)

; A plural type name means a list of the type.

; exp =
(define-const-structure (And    exps))
(define-const-structure (App    exp exps))
(define-const-structure (Begin  exps))
(define-const-structure (Const  val pred))
(define-const-structure (If     exp1 exp2 exp3))
(define-const-structure (Lam    names body))
(define-const-structure (Let    binds body))
(define-const-structure (Let*   binds body))
(define-const-structure (Letr   binds body))
(define-const-structure (Or     exps))
(define-const-structure (Prim   name))
(define-const-structure (Delay  exp))
(define-const-structure (Set!   name exp))
(define-const-structure (Var    name))
(define-const-structure (Vlam   names name body))
(define-const-structure (Match  exp mclauses))
(define-const-structure (Record binds))
(define-const-structure (Field  name exp))
(define-const-structure (Cast   type exp))

(define-const-structure (Body   defs exps))

(define-const-structure (Bind   name exp))

(define-const-structure (Variant con pred arg-types))

(define-const-structure (Mclause pat Body fail))

; pat =
(define-const-structure (Pconst name pred)); eg. 1, "hi", but not #t, #f, or ()
(define-const-structure (Pvar   name))     ; pattern variable
(define-const-structure (Pobj   name pats)); composite, eg. (cons p1 p2)
(define-const-structure (Pany))            ; _
(define-const-structure (Pelse))           ; else
(define-const-structure (Pand   pats))     ; (length pats) >= 1
(define-const-structure (Ppred  name))     ; eg. (? exp)
(define-const-structure (Pnot   pat))      ; eg. (not pat)

(define-const-structure (Iclause exp binds))

; definitions =
(define-const-structure (Define name box-exp))
(define-const-structure (Defstruct tag args make pred get set mutable))
(define-const-structure (Datatype _))
; _ = (list (cons (list symbol) (list (Variant name (list type)))))))

(define-structure       (Name name         ; name
                              ty           ; type
                              occ          ; # of occurrences
                              mutated      ; set!'ed?
                              gdef         ; user defined global name
                              primitive    ; primitive
                              struct       ; built by define-structure
                              pure         ; pure constant
                              predicate    ; type template if predicate
                              variant      ; type template for variant
                              selector     ; match template if selector
                              subtyped     ; Remy subtyping is enough
                              ))

; Decorations inserted by the type checker.
(define-structure (Type   ty exp))
(define-const-structure (Shape  _ _))
(define-const-structure (Check  _ _))
