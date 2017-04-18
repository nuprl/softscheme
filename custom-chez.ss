;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization file for Chez Scheme.

;; Inform Soft Scheme of the types of the primitives.

(st:check
  '(begin
     ;; Standard Chez Scheme primitives.
     (define-primitive call/cc       (((a -> b) -> a) -> a))
     (define-primitive add1          (num -> num)                                      pure)
     (define-primitive sub1          (num -> num)                                      pure)
     (define-primitive 1+            (num -> num)                                      pure)
     (define-primitive 1-            (num -> num)                                      pure)
     (define-primitive ormap         ((a -> (+ false b)) (list a) -> (+ false b)))
     (define-primitive andmap        ((a -> (+ true b)) (list a) -> (+ true b)))
     (define-primitive make-list     (num a -> (list a))                               pure-if-cons-is)
     (define-primitive format        ((arg str a) ->* str))
     (define-primitive error         ((arg a (arg str b)) ->* c)                       pure)
     (define-primitive printf        ((arg str a) ->* void))
     (define-primitive pretty-print  ((arg a (+ noarg (arg oport noarg))) ->* void))
     (define-primitive gensym        (-> sym)                                          pure)
     (define-primitive sort          ((a a -> (+ false b)) (list a) -> (list a)))
     (define-primitive dynamic-wind  ((-> a) (-> b) (-> c) -> b))
     (define-primitive expand-once   (s-exp -> s-exp))
     (define-primitive append!       ((arglist (list a)) ->* (list a))                 mutates-cons)
     ;; Rice Scheme things.
     (define-primitive abort         (a -> b)                                          pure))
  #f)

;; Build CHECK versions of the primitives (see checklib.ss for macros).

(define CHECK-call/cc CHECK-call-with-current-continuation)
(make-CHECK-typed add1 number?)
(make-CHECK-typed sub1 number?)
(make-CHECK-typed 1+ number?)
(make-CHECK-typed 1- number?)
(define CHECK-ormap
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (<= 2 (length a)) (procedure? (car a)) (andmap list? (cdr a)))
          (apply ormap a)
          (st:failure `(CHECK-ormap ,@id) "invalid arguments: ~a" a)))))
(define CHECK-andmap
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (<= 2 (length a)) (procedure? (car a)) (andmap list? (cdr a)))
          (apply andmap a)
          (st:failure `(CHECK-andmap ,@id) "invalid arguments: ~a" a)))))
(make-CHECK-typed make-list number? (lambda (_) #t))
(define CHECK-format
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (pair? a) (string? (car a)))
          (apply format a)
          (st:failure `(CHECK-format ,@id) "invalid arguments: ~a" a)))))
(define CHECK-error
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (pair? a) (pair? (cdr a)) (string? (cadr a)))
          (apply error a)
          (st:failure `(CHECK-error ,@id) "invalid arguments: ~a" a)))))
(define CHECK-printf
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (pair? a) (string? (car a)))
          (apply printf a)
          (st:failure `(CHECK-printf ,@id) "invalid arguments: ~a" a)))))
(define CHECK-pretty-print
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (pair? a) (or (null? (cdr a)) (output-port? (cadr a))))
          (apply pretty-print a)
          (st:failure `(CHECK-pretty-print ,@id) "invalid arguments: ~a" a)))))
(make-CHECK-typed gensym)
(make-CHECK-typed sort procedure? list?)
(make-CHECK-typed dynamic-wind procedure? procedure? procedure?)
(make-CHECK-onearg expand-once)
(define CHECK-append!
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (let loop ((b a))
        (match b
          (() #t)
          ((l) #t)
          (((? list?) . y) (loop y))
          (_ (st:failure `(CHECK-append! ,@id) "invalid arguments: ~a" a))))
      (apply append! a))))
(make-CHECK-onearg abort)
