;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Topological Sort - sort a list of "elt" according to how they refer
;; to "name"s.  Return a list of strongly connected components, such
;; that components refer only to ones earlier in the list.
;;
;; Input:
;;   graph         : list elt
;;   name-of       : elt -> name
;;   references-of : elt -> set name
;;
;; Output          : list list elt
;;
;; If graph has duplicate elements (ie. (map name-of graph) has repeated
;; members) then all but the first element will be discarded.  elt and
;; name must be disjoint.

;; A graph is represented as a list of:
;;  (node:name nay:(box (list name)) mark:(box bool) name:(+ elt name)
;;
;; "nay" is an adjacency list of neighbours.
;; This representation allows lookup of a node by assq.
;;
;; For GT (G transpose), name field is a name.  For G, its an elt,
;; since we have to translate back to elts.
;;
;; See "Introduction to Algorithms" by Cormen, Leiserson, and Rivest,
;;  McGraw Hill, 1990.
       
(define top-sort
  (lambda (graph name-of references-of)
    (let*
      ((adj assq)
       (G (map (lambda (x)
                 (list (name-of x) (box (references-of x)) (box #f) x))
               graph))
       (GT (let ((GT (map (match-lambda
                            ((n _ _ name)
                             (list n (box empty-set) (box #f) n)))
                          G)))
             (for-each
               (match-lambda
                 ((n nay _ _)
                  (for-each
                    (lambda (v)
                      (match (adj v GT)
                        (#f #f)
                        ((_ b _ _) (set-box! b (cons n (unbox b))))))
                    (unbox nay))))
               G)
             GT))
       (visit (lambda (vG)
                ; Graph -> (node nay mark name) x (list name) -> (list name)
                ; Traverse vG, starting at node, depth-first, marking in vG.
                ; Return a list of the nodes traversed, in pre-order.
                (letrec ((visit
                          (lambda (g l)
                            (match g
                              (#f l)
                              ((n nay mark name)
                               (if (unbox mark)
                                   l
                                   (begin
                                     (set-box! mark #t)
                                     (cons name
                                           (foldr (lambda (v l)
                                                    (visit (adj v vG) l))
                                             l
                                             (unbox nay))))))))))
                  visit)))
       (visit-GT (visit GT))
       (visit-G (visit G))
       (post (foldr visit-GT () GT))
       (pre (foldl (lambda (gg l)
                     (match (visit-G (adj gg G) ())
                       (() l)
                       (c (cons c l))))
              ()
              post)))
      (reverse pre))))
