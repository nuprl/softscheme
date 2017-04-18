;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Decoding tidy types into presentation types

(define tidy
  (match-lambda
    (($ TS _ t) (tidy-print t print-union assemble-union #f))
    (t (tidy-print t print-union assemble-union #f))))

(define ptype
  (match-lambda
    (($ TS _ t) (tidy-print t print-raw-union assemble-raw-union #t))
    (t (tidy-print t print-raw-union assemble-raw-union #t))))

(define tidy-print
  (lambda (t print assemble top)
    (let* ((share (shared-unions t top))
           (bindings (map-with-n
                       (lambda (t n)
                         (list t (box #f) (box #f) (symbol-append "Y" (+ 1 n))))
                       share))
           (body (print t (print-binding bindings)))
           (let-bindings (filter-map
                           (match-lambda
                             ((_ _ ($ box #f) _) #f)
                             ((_ ($ box t) ($ box x) _) (list x t)))
                           bindings)))
      (assemble let-bindings body))))

(define print-binding
  (lambda (bindings)
    (lambda (ty share-wrapper var-wrapper render)
      (match (assq ty bindings)
        (#f (render))
        ((_ box-tprint box-name nprint)
         (var-wrapper
           (or (unbox box-name)
               (begin
                 (set-box! box-name nprint)
                 (set-box! box-tprint (share-wrapper (render)))
                 nprint))))))))

; Build a list of shared non-variable types.
; If all is #f, only types with empty LABELS are considered.
(define shared-unions
  (lambda (t all)
    (let ((seen ()))
      (recur loop ((t t)(top #t))
        (match t
          (($ box (? V?)) #f)
          (($ box ($ C _ _ a n))
           (match (and top (assq t seen))
             (#f (set! seen (cons (cons t (box 1)) seen))
                 (for-each (lambda (x) (loop x #t)) a)
                 (loop n all))
             ((_ . b) (set-box! b (+ 1 (unbox b))))))
          (($ box i) (loop i top))
          ('Top #f)))
      (reverse
        (filter-map
          (match-lambda ((_ . ($ box 1)) #f)((t . _) t))
          seen)))))

(define print-raw-union   ; traversal order must match that of shared-unions
  (lambda (t print-share)
    (recur loop ((t t))
      (match t
        (($ box (? V?)) (pvar t))
        (($ box ($ C x p a n))
         (print-share t (lambda (x) x) (lambda (x) x)
           (lambda ()
             (let ((pr-x `(,(K-name x) ,(loop p) ,@(mapLR loop a))))
               (cons pr-x (loop n))))))
        (($ box i) (loop i))
        ('Top '+)))))

(define assemble-raw-union
  (lambda (bindings body)
    (if (null? bindings)
        body
        `(rec ,bindings ,body))))

(define print-union   ; traversal order must match that of shared-unions
  (lambda (t print-share)
    (add-+
      (recur loop ((t t)(tailvis (visible? (tailvar t))))
        (match t
          (($ box (? V?))
           (if (visible? t)
               (list (pvar t))
               ()))
          (($ box ($ C x p a n))
           (print-share t add-+ list
             (lambda ()
               (cond
                 ((visible? p)
                  (cons
                    (cond
                      ((null? a)
                       (K-name x))
                      ((eq? '->* (K-name x))
                       (let ((arg (add-+
                                    (loop (car a)
                                          (visible? (tailvar (car a))))))
                             (res (add-+
                                    (loop (cadr a)
                                          (visible? (tailvar (cadr a)))))))
                         (if (finite-arg-list? arg)
                             `(,@(squish-arg-list arg) -> ,res)
                             `(,arg ->* ,res))))
                      ((eq? 'record (K-name x))
                       `(record ,@(loop (car a) #f)))
                      (else
                       `(,(K-name x)
                         ,@(mapLR
                             (lambda (x)
                               (add-+ (loop x (visible? (tailvar x)))))
                             a))))
                    (loop n tailvis)))
                 ((not tailvis)
                  (loop n tailvis))
                 (else
                  (cons `(not ,(K-name x))
                        (loop n tailvis)))))))
          (($ box i) (loop i tailvis)))))))

(define assemble-union
  (lambda (bindings body)
    (subst-small-type (map clean-binding bindings) body)))

(define add-+
  (match-lambda
    (() 'EMPTY)
    ((t) t)
    (x (cons '+ x))))

(define tailvar
  (match-lambda
    ((and x ($ box (? V?))) x)
    (($ box ($ C _ _ _ n))  (tailvar n))
    (($ box i)              (tailvar i))))

(define finite-arg-list?
  (match-lambda
    ('noarg #t)
    (('arg _ b) (finite-arg-list? b))
    (_ #f)))

(define squish-arg-list
  (match-lambda
    ('noarg ())
    (('arg a b) (cons a (squish-arg-list b)))))

(define clean-binding
  (lambda (binding)
    (match binding
      ((u ('+ 'nil ('cons a v)))
       (if (and (equal? u v) (not (memq* u a)))
           (list u `(list ,a))
           binding))
      ((u ('+ ('cons a v) 'nil))
       (if (and (equal? u v) (not (memq* u a)))
           (list u `(list ,a))
           binding))
; this pattern only needed if doing non-top-level sharing
;      ((u ('cons a ('+ 'nil v)))
;       (if (and (equal? u v) (not (memq* u a)))
;           (list u `(cons ,a (list ,a)))
;           binding))
      ((u ('+ 'noarg ('arg a v)))
       (if (and (equal? u v) (not (memq* u a)))
           (list u `(arglist ,a))
           binding))
      ((u ('+ ('arg a v) 'noarg))
       (if (and (equal? u v) (not (memq* u a)))
           (list u `(arglist ,a))
           binding))
; this pattern only needed if doing non-top-level sharing
;      ((u ('arg a ('+ 'noarg v)))
;       (if (and (equal? u v) (not (memq* u a)))
;           (list u `(arg ,a (arglist ,a)))
;           binding))
      (x x))))

(define memq*
  (lambda (v t)
    (recur loop ((t t))
      (match t
        ((x . y) (or (loop x) (loop y)))
        (_ (eq? v t))))))

(define subst*    ; substitution for trees
  (lambda (new old t)
    (cond ((eq? old t) new)
          ((pair? t) (cons (subst* new old (car t))
                           (subst* new old (cdr t))))
          (else t))))

(define subst-small-type
  (lambda (bindings body)
    (recur loop ((bindings bindings)(newb ())(body body))
      (match bindings
        (() (let ((newb
                   (filter
                     (match-lambda ((name type) (not (equal? name type))))
                     newb)))
              (if (null? newb)
                  body
                  `(rec ,(reverse newb) ,body))))
        (((and b (name type)) . rest)
         (if (and (not (memq* name type)) (small-type? type))
             (loop (subst* type name rest)
                   (subst* type name newb)
                   (subst* type name body))
             (loop rest (cons b newb) body)))))))

(define small-type?
  (lambda (t)
    (>= 8 (recur loop ((t t))
            (match t
              ('+ 0)
              ((? symbol? s) 1)
              ((? number? n) 0)
              ((x . y) (+ (loop x) (loop y)))
              (() 0))))))
