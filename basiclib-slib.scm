;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file contains things that the type checker assumes are
;; defined by the underlying Scheme implementation.  SLIB version.

(require 'format)

;; Used by many macros. 
(define symbol-append
  (lambda l
    (string->symbol
      (apply string-append (map (lambda (x) (format #f "~a" x)) l)))))
(define gensym gentemp)

;; two predicates missing from Scheme
(define true-object?
  (lambda (x) (eq? #t x)))
(define false-object?
  (lambda (x) (eq? #f x)))

;; void *should* construct a distinct, special value.
(define void (lambda () (cond (#f #f))))

;; CHANGE the semantics of when and unless.
;; Both return (void), regardless of whether the test succeeds or fails.
(defmacro when args
  (match args
    ((tst body ..1)
     `(if ,tst (begin ,@body (void)) (void)))))
(defmacro unless args
  (match args
    ((tst body ..1)
     `(if ,tst (void) (begin ,@body (void))))))

;; constant vectors
(define make-cvector make-vector)
(define cvector vector)
(define cvector-length vector-length)
(define cvector-ref vector-ref)
(define cvector->list vector->list)
(define list->cvector list->vector)

;; records

(define-const-structure (record _))

(defmacro record args
  (match args
    ((((? symbol? id) exp) ...)
     `(make-record (list ,@(map (lambda (i x) `(cons ',i ,x)) id exp))))
    (_ (slib:error "syntax error at " `(record ,@args)))))

(defmacro field args
  (match args
    (((? symbol? id) exp)
     `(match ,exp
        (($ record x) (match (assq ',id x)
                        (#f (slib:error "no field " ,id 'in 
                              (cons 'record (map car x))))
                        ((_ . x) x)))
        (_ (slib:error "not a record: " '(field ,id _)))))
    (_ (slib:error "syntax error at " `(field ,@args)))))

;; modules

(define-const-structure (module _))

(defmacro module args
  (match args
    (((I ...) defs ...)
     `(let () ,@defs
        (make-module
          (record ,@(map (lambda (x) (list x x)) I)))))
    (_ (slib:error "syntax error at " `(module ,@args)))))

(defmacro import args
  (match args
    ((((mod defs ...) ...) body ..1)
     (let* ((m (map (lambda (_) (gentemp)) mod))
            (newdefs
             (let Loop ((mod-names m)(l-defs defs))
               (if (null? mod-names)
                   '()
                   (append
                     (let ((m (car mod-names)))
                       (map
                         (match-lambda
                           ((? symbol? x) `(,x (field ,x ,m)))
                           (((? symbol? i) (? symbol? e)) `(,i (field ,e ,m)))
                           (x (slib:error "ill-formed definition: " x)))
                         (car l-defs)))
                     (Loop (cdr mod-names) (cdr l-defs)))))))
       `(let ,(map (lambda (m mod)
                     `(,m (match ,mod (($ module x) x))))
                   m mod)
          (let ,newdefs body ...))))))

;; exceptions

(define raise
  (lambda vals
    (slib:error "Unhandled exception " vals)))

(defmacro fluid-let args
  (match args
    ((((x val) ...) body ..1)
     (let ((old-x (map (lambda (_) (gentemp)) x))
           (swap-x (map (lambda (_) (gentemp)) x))
           (swap (gentemp)))
       `(let ,(map list old-x val)
          (let ((,swap
                  (lambda ()
                    (let ,(map list swap-x old-x)
                      ,@(map (lambda (old x) `(set! ,old ,x)) old-x x)
                      ,@(map (lambda (x swap) `(set! ,x ,swap)) x swap-x)))))
            (dynamic-wind ,swap (lambda () ,@body) ,swap)))))
    (_ (slib:error "syntax error at " `(fluid-let ,@args)))))
                 
(defmacro handle args
  (match args
    ((e h)
     (let ((k (gentemp))
           (exn (gentemp)))
       `((call-with-current-continuation
           (lambda (k)
             (fluid-let ((raise (lambda ,exn (k (lambda () (apply ,h ,exn))))))
               (let ((v ,e))
                 (lambda () v))))))))
    (_ (slib:error "syntax error in " `(handle ,@args)))))

;; type annotations

(defmacro : args
  (match args
    ((typeexp exp) exp)))

(defmacro module: args
  (match args
    ((((I type) ...) defs ...)
     `(let () ,@defs
        (make-module
          (record
            ,@(map (lambda (I type) `(,I (: ,type ,I))) I type)))))))

(defmacro define: args
  (match args
    ((name type exp)
     `(define ,name (: ,type ,exp)))))

;; primitive definitions for type checker

(defmacro define-primitive args
  (match args
    ((define-primitive (? symbol? name) type)
     #f)))

;; for primitive failure

(define st:failure
  (lambda (chk fmt . args)
    (slib:error (apply format #f (string-append "~a : " fmt) chk args))))
