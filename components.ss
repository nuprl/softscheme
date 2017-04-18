;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sort a set of defines into strongly connected components.
;;
;; A graph is represented as a list of:
;;  (node:Name nay:(list Name) mark:(box bool) name:(+ Name Define)
;;
;; "nay" is an adjacency list of neighbours.
;; This representation allows lookup of a node by assq.
;;
;; For GT (G transpose), name is a Name.  For G, its a Define,
;; since we have to translate back to Defines.
;;

(define make-components
  (lambda (d)
    (let*
      ((structs (filter-map (match-lambda
                              ((? Define?) #f)
                              (x x))
                  d))
       (defs (filter-map (match-lambda
                           ((? Define? x) x)
                           (_ #f))
               d))
       (name-of (match-lambda (($ Define x _) x)))
       (ref-of (match-lambda (($ Define _ box-e)
                              (references (unbox box-e) Name-gdef))))
       (comp (top-sort defs name-of ref-of)))
      (when #f
        (printf "Components:~%")
        (pretty-print (map (lambda (c)
                             (map (match-lambda
                                    (($ Define x _) (and x (Name-name x))))
                                  c))
                           comp)))
      (append structs comp))))

(define make-body-components
  (lambda (d)
    (let*
      ((structs (filter-map (match-lambda
                              ((? Define?) #f)
                              (x x))
                  d))
       (defs (filter-map (match-lambda
                           ((? Define? x) x)
                           (_ #f))
               d))
       (name-of (match-lambda (($ Define x _) x)))
       (bound (map name-of defs))
       (ref-of (match-lambda
                 (($ Define _ box-e)
                  (references (unbox box-e) (lambda (x) (memq x bound))))))
       (comp (top-sort defs name-of ref-of)))
      (when #f
        (printf "Components:~%")
        (pretty-print (map (lambda (c)
                             (map (match-lambda
                                    (($ Define x _) (and x (Name-name x))))
                                  c))
                           comp)))
      (append structs comp))))

(define make-letrec-components
  (lambda (bindings)
    (let*
      ((name-of Bind-name)
       (bound (map name-of bindings))
       (ref-of (match-lambda
                 (($ Bind _ e) (references e (lambda (x) (memq x bound))))))
       (comp (top-sort bindings name-of ref-of)))
      (when #f
        (printf "Letrec Components:~%")
        (pretty-print (map (lambda (c)
                             (map (match-lambda
                                    (($ Bind x _) (pname x)))
                                  c))
                           comp)))
      comp)))

(define references
  (lambda (e ref?)
    (recur loop ((e e))
      (match e
        (($ Check _ e) (loop e))
        (($ Type _ e) (loop e))
        (($ Shape _ e) (loop e))
        (($ Define x box-e)
         (if (and x (Name-mutated x))
             (union (set x) (loop (unbox box-e)))
             (loop (unbox box-e))))
        ((? Defstruct?) empty-set)
        ((? Datatype?) empty-set)
        ((? Const?) empty-set)
        (($ Var x)
         (if (ref? x)
             (set x)
             empty-set))
        (($ Lam _ e1)
         (loop e1))
        (($ Vlam _ _ e1)
         (loop e1))
        (($ App e0 args)
         (foldr union2 (loop e0) (map loop args)))
        (($ Let b e2)
         (let ((do-bind (match-lambda
                          (($ Bind _ e) (loop e)))))
           (foldr union2 (loop e2) (map do-bind b))))
        (($ Let* b e2)
         (let ((do-bind (match-lambda
                          (($ Bind _ e) (loop e)))))
           (foldr union2 (loop e2) (map do-bind b))))
        (($ Letr b e2)
         (let ((do-bind (match-lambda
                          (($ Bind _ e) (loop e)))))
           (foldr union2 (loop e2) (map do-bind b))))
        (($ Body defs exps)
         (foldr union2 empty-set (map loop (append defs exps))))
        (($ Record b)
         (let ((do-bind (match-lambda
                          (($ Bind _ e) (loop e)))))
           (foldr union2 () (map do-bind b))))
        (($ Field _ e)
         (loop e))
        (($ Cast _ e)
         (loop e))
        (($ And exps)
         (foldr union2 empty-set (map loop exps)))
        (($ Or exps)
         (foldr union2 empty-set (map loop exps)))
        (($ Begin exps)
         (foldr union2 empty-set (map loop exps)))
        (($ If test then els)
         (union (loop test) (loop then) (loop els)))
        (($ Delay e)
         (loop e))
        (($ Set! x body)
         (union (if (ref? x)
                    (set x)
                    empty-set)
           (loop body)))
        (($ Match exp clauses)
         (foldr
           union2
           (loop exp)
           (map (match-lambda (($ Mclause _ exp _) (loop exp))) clauses)))))))

