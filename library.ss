;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc useful functions

(define set-vector! vector-set!)
(define set-string! string-set!)

;; map from left to right
(define mapLR
  (lambda (f l)
    (match l
      (() ())
      ((x . y) (let ((v (f x))) (cons v (mapLR f y)))))))

;; map from right to left
(define mapRL
  (lambda (f l)
    (match l
      (() ())
      ((x . y) (let ((v (mapRL f y))) (cons (f x) v))))))

;; fold a binary function down a list, left to right
;; right operand of f is accumulator
(define foldl
  (lambda (f i l)
    (recur loop ((l l)(acc i))
      (match l
        (() acc)
        ((x . y) (loop y (f x acc)))))))

;; fold a binary function down a list, right to left
;; right operand of f is accumulator
(define foldr
  (lambda (f i l)
    (recur loop ((l l))
      (match l
        (() i)
        ((x . y) (f x (loop y)))))))

;; filter elements out of a list by a predicate
(define filter
  (lambda (p l)
    (match l
      (() ())
      ((x . y) (if (p x) (cons x (filter p y)) (filter p y))))))

;; filter and map left to right
(define filter-map
  (lambda (p l)
    (match l
      (() ())
      ((x . y)
       (match (p x)
         (#f (filter-map p y))
         (x (cons x (filter-map p y))))))))

;; last element of a list
(define rac
  (lambda (l)
    (match l
      ((last) last)
      ((_ . rest) (rac rest)))))

;; all but the last element of a list
(define rdc
  (lambda (l)
    (match l
      ((_) ())
      ((x . rest) (cons x (rdc rest))))))

;; map left to right, but also pass f a 0-based index
(define map-with-n
  (lambda (f l)
    (recur loop ((l l)(n 0))
      (match l
        (() ())
        ((x . y) (let ((v (f x n))) (cons v (loop y (+ 1 n)))))))))

;; return a list of the s-expressions in a file
(define readfile
  (lambda (f)
    (with-input-from-file f
      (letrec ((rf (lambda ()
                     (match (read)
                       ((? eof-object?) ())
                       (sexp (cons sexp (rf)))))))
        rf))))

;; map a binary function down 2 lists, left to right
(define map2
  (lambda (f a b)
    (match (cons a b)
      ((() . ())
       ())
      (((ax . ay) . (bx . by))
       (let ((v (f ax bx))) (cons v (map2 f ay by))))
      (else (error 'map2 "lists differ in length")))))

;; interate a binary function down 2 lists, left to right
(define for-each2
  (lambda (f a b)
    (match (cons a b)
      ((() . ())
       (void))
      (((ax . ay) . (bx . by))
       (f ax bx)
       (for-each2 f ay by))
      (else (error 'for-each2 "lists differ in length")))))

;; andmap for 2 lists
(define andmap2
  (lambda (f a b)
    (match (cons a b)
      ((() . ())
       (and))
      (((ax) . (bx))
       (f ax bx))
      (((ax . ay) . (bx . by))
       (and (f ax bx) (andmap2 f ay by)))
      (else (error 'andmap2 "lists differ in length")))))

;; ormap for 2 lists
(define ormap2
  (lambda (f a b)
    (match (cons a b)
      ((() . ())
       (or))
      (((ax) . (bx))
       (f ax bx))
      (((ax . ay) . (bx . by))
       (or (f ax bx) (ormap2 f ay by)))
      (else (error 'ormap2 "lists differ in length")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set operations implemented by lists.
;; Identity of elements is based on eq?.
;; These should probably be sped up some day.

(define empty-set ())

(define empty-set? null?)

;; construct a set
(define set
  (lambda l
    (list->set l)))

;; construct a set from a list by removing duplicates
(define list->set
  (match-lambda
    (() ())
    ((x . y) (if (memq x y)
                 (list->set y)
                 (cons x (list->set y))))))

;; test for membership
(define element-of?
  (lambda (x set)
    (and (memq x set) #t)))

(define cardinality length)

;; does s2 contain s1?
(define set<=
  (lambda (a b)
    (and (andmap (lambda (x) (memq x b)) a) #t)))

;; are two sets equal? (mutually containing)
(define set-eq?
  (lambda (a b)
    (and (= (cardinality a) (cardinality b)) (set<= a b))))

;; unite two sets
(define union2
  (lambda (a b)
    (if (null? b)
        a
        (foldr (lambda (x b)
                 (if (memq x b)
                     b
                     (cons x b)))
          b
          a))))

;; unite any number of sets
(define union
  (lambda l
    (foldr union2 () l)))
