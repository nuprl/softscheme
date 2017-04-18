;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file contains things that the type checker assumes are
;; defined by the underlying Scheme implementation.


;; Used by many macros. 
(eval-when (compile load eval)
  (define symbol-append
    (lambda l
      (string->symbol
        (apply string-append (map (lambda (x) (format "~a" x)) l))))))

;; two predicates missing from Scheme
(define true-object?
  (lambda (x) (eq? #t x)))
(define false-object?
  (lambda (x) (eq? #f x)))

;; CHANGE the semantics of when and unless.
;; Both return (void), regardless of whether the test succeeds or fails.
(extend-syntax (when)
  ((when tst body0 body ...)
   (if tst (begin body0 body ... (#%void)) (#%void))))
(extend-syntax (unless)
  ((unless tst body0 body ...)
   (if tst (#%void) (begin body0 body ... (#%void)))))

;; constant vectors
(define make-cvector make-vector)
(define cvector vector)
(define cvector-length vector-length)
(define cvector-ref vector-ref)
(define cvector->list vector->list)
(define list->cvector list->vector)

;; records

(define-const-structure (record _))

(extend-syntax (record)
  ((record (id exp) ...)
   (andmap symbol? '(id ...))
   (make-record
     `((id . ,exp) ...))))

(extend-syntax (field)
  ((field id exp)
   (symbol? 'id)
   (match exp
     (($ record x) (match (#%assq 'id x)
                     (#f (#%error '(field id _) "no such field in ~a"
                           (#%cons 'record (#%map #%car x))))
                     ((_ . x) x)))
     (_ (#%error '(field id _) "not a record")))))

;; modules

(define-const-structure (module _))

(extend-syntax (module)
  ((module (I ...) defs ...)
   (let () defs ...
     (make-module
       (record (I I) ...)))))

(extend-syntax (import)
  ((import ((mod defs ...) ...) body ...)
   (with (((m ...) (map (lambda (_) (gensym)) '(mod ...))))
     (with ((newdefs
             (recur Loop ((mod-names '(m ...))(l-defs '((defs ...) ...)))
               (if (null? mod-names)
                   '()
                   (append
                     (let ((m (car mod-names)))
                       (map
                         (match-lambda
                           ((? symbol? x) `(,x (field ,x ,m)))
                           (((? symbol? i) (? symbol? e)) `(,i (field ,e ,m)))
                           (x (error 'import "ill-formed definition: ~a" x)))
                         (car l-defs)))
                     (Loop (cdr mod-names) (cdr l-defs)))))))
       (let ((m (match mod (($ module x) x))) ...)
         (let newdefs body ...))))))

;; exceptions

(define raise
  (lambda vals
    (#%error #f "Unhandled exception ~a" vals)))

(extend-syntax (handle)
  ((handle e h)
   (with ((k (gensym))(exn (gensym)))
     ((call/cc
        (lambda (k)
          (fluid-let ((raise (lambda exn (k (lambda () (apply h exn))))))
            (let ((v e))
              (lambda () v)))))))))

;; type annotations

(extend-syntax (:)
  ((: typeexp exp)
   exp))

(extend-syntax (module:)
  ((module: ((I type) ...) defs ...)
   (let () defs ...
     (make-module
       (record (I (: type I)) ...)))))

(extend-syntax (define:)
  ((define: name type exp)
   (define name (: type exp))))

;; primitive definitions for type checker

(extend-syntax (define-primitive)
  ((define-primitive name type)
   (symbol? 'name)
   #f)
  ((define-primitive name type mode)
   (symbol? 'name)
   #f))

;; for primitive failure

(define st:failure #%error)
