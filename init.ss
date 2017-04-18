;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initial environments, etc.

(define qop (lambda (s) (string->symbol (string-append "# " s))))
(define Qcons (qop "cons"))
(define Qbox (qop "box"))
(define Qlist (qop "list"))
(define Qvector (qop "vector"))

;; All primitives must be procedures.

(define initial-info
  ;  name           type        optional-attribute(s)
  ; where optional-attribute(s) are any of:
  ;  (S selector-pattern)   is a selector for match
  ;  (P predicate-pattern)  is a predicate for match
  ;  (I)                    impure
  ;  (Ic)                   impure if cons is impure
  ;  (T)                    Remy subtyped
  ;  (D)                    no DEFINITE check
  ;  (N)                    no check at all
  ;  (C)                    implementation contains a check
  ; Note: things with mutable inputs are also marked impure.
  `(; booleans
    (not            (a -> bool)                                          (T))

    ; equivalence predicates
    (eqv?           (a b -> bool)                                        (T))
    (eq?            (a b -> bool)                                        (T))
    (equal?         (a b -> bool)                                        (T))

    ; pairs and lists
    (cons           (a b -> (cons a b))                                  (T) (Ic))
    (car            ((cons a b) -> a)                                    (T) (S (X . _)))
    (cdr            ((cons b a) -> a)                                    (T) (S (_ . X)))
    (caar          ((cons (cons a b) c) -> a)                            (T) (S ((X . _) . _)))
    (cadr          ((cons c (cons a b)) -> a)                            (T) (S (_ X . _)))
    (cdar          ((cons (cons b a) c) -> a)                            (T) (S ((_ . X) . _)))
    (cddr          ((cons c (cons b a)) -> a)                            (T) (S (_ _ . X)))
    (caaar         ((cons (cons (cons a b) c) d) -> a)                   (T) (S (((X . _) . _) . _)))
    (caadr         ((cons d (cons (cons a b) c)) -> a)                   (T) (S (_ (X . _) . _)))
    (cadar         ((cons (cons c (cons a b)) d) -> a)                   (T) (S ((_ X . _) . _)))
    (caddr         ((cons d (cons c (cons a b))) -> a)                   (T) (S (_ _ X . _)))
    (cdaar         ((cons (cons (cons b a) c) d) -> a)                   (T) (S (((_ . X) . _) . _)))
    (cdadr         ((cons d (cons (cons b a) c)) -> a)                   (T) (S (_ (_ . X) . _)))
    (cddar         ((cons (cons c (cons b a)) d) -> a)                   (T) (S ((_ _ . X) . _)))
    (cdddr         ((cons d (cons c (cons b a))) -> a)                   (T) (S (_ _ _ . X)))
    (caaaar        ((cons (cons (cons (cons a b) c) d) e) -> a)          (T) (S ((((X . _) . _) . _) . _)))
    (caaadr        ((cons e (cons (cons (cons a b) c) d)) -> a)          (T) (S (_ ((X . _) . _) . _)))
    (caadar        ((cons (cons d (cons (cons a b) c)) e) -> a)          (T) (S ((_ (X . _) . _) . _)))
    (caaddr        ((cons e (cons d (cons (cons a b) c))) -> a)          (T) (S (_ _ (X . _) . _)))
    (cadaar        ((cons (cons (cons c (cons a b)) d) e) -> a)          (T) (S (((_ X . _) . _) . _)))
    (cadadr        ((cons e (cons (cons c (cons a b)) d)) -> a)          (T) (S (_ (_ X . _) . _)))
    (caddar        ((cons (cons d (cons c (cons a b))) e) -> a)          (T) (S ((_ _ X . _) . _)))
    (cadddr        ((cons e (cons d (cons c (cons a b)))) -> a)          (T) (S (_ _ _ X . _)))
    (cdaaar        ((cons (cons (cons (cons b a) c) d) e) -> a)          (T) (S ((((_ . X) . _) . _) . _)))
    (cdaadr        ((cons e (cons (cons (cons b a) c) d)) -> a)          (T) (S (_ ((_ . X) . _) . _)))
    (cdadar        ((cons (cons d (cons (cons b a) c)) e) -> a)          (T) (S ((_ (_ . X) . _) . _)))
    (cdaddr        ((cons e (cons d (cons (cons b a) c))) -> a)          (T) (S (_ _ (_ . X) . _)))
    (cddaar        ((cons (cons (cons c (cons b a)) d) e) -> a)          (T) (S (((_ _ . X) . _) . _)))
    (cddadr        ((cons e (cons (cons c (cons b a)) d)) -> a)          (T) (S (_ (_ _ . X) . _)))
    (cdddar        ((cons (cons d (cons c (cons b a))) e) -> a)          (T) (S ((_ _ _ . X) . _)))
    (cddddr        ((cons e (cons d (cons c (cons b a)))) -> a)          (T) (S (_ _ _ _ . X)))
    (set-car!       ((cons a b) a -> void)                               (T))
    (set-cdr!       ((cons a b) b -> void)                               (T))
    (list           ((arglist a) ->* (list a))                               (Ic))
    (length         ((list a) -> num)                                    )
    (append         ((arglist (list a)) ->* (list a))                        (Ic) (D))
    (reverse        ((list a) -> (list a))                                   (Ic))
    (list-tail      ((list a) num -> (list a))                                   (C))
    (list-ref       ((list a) num -> a)                                          (C))
    (memq           (a (list b) -> (+ false (cons b (list b))))          )
    (memv           (a (list b) -> (+ false (cons b (list b))))          )
    (member         (a (list b) -> (+ false (cons b (list b))))          )
    (assq           (a (list (cons b c)) -> (+ false (cons b c)))        )
    (assv           (a (list (cons b c)) -> (+ false (cons b c)))        )
    (assoc          (a (list (cons b c)) -> (+ false (cons b c)))        )

;    types for faster versions that do less argument checking
;    (list-tail      ((MU x (+ nil (cons a x) b)) num ->
;                        (MU y (+ nil (cons a y) b)))                    )
;    (list-ref       ((MU x (+ nil (cons a x) b)) num -> a)              )
;    (memq           (a (MU x (+ nil (cons b x) c)) ->
;                        (+ false (cons b (MU y (+ nil (cons b y) c))))) )
;    (memv           (a (MU x (+ nil (cons b x) c)) ->
;                        (+ false (cons b (MU y (+ nil (cons b y) c))))) )
;    (member         (a (MU x (+ nil (cons b x) c)) ->
;                        (+ false (cons b (MU y (+ nil (cons b y) c))))) )
;    (assq           (a (MU x (+ nil (cons (+ (cons b c) d) x) e)) ->
;                        (+ false (cons b c)))                           )
;    (assv           (a (MU x (+ nil (cons (+ (cons b c) d) x) e)) ->
;                        (+ false (cons b c)))                           )
;    (assoc          (a (MU x (+ nil (cons (+ (cons b c) d) x) e)) ->
;                        (+ false (cons b c)))                           )
    
    ; symbols
    (symbol->string (sym -> str)                                         (T))
    (string->symbol (str -> sym)                                         (T))

    ; numbers
    (complex?       (a -> bool)                                          (T))
    (real?          (a -> bool)                                          (T))
    (rational?      (a -> bool)                                          (T))
    (integer?       (a -> bool)                                          (T))
    (exact?         (num -> bool)                                        (T))
    (inexact?       (num -> bool)                                        (T))
    (=              ((arg num (arg num (arglist num))) ->* bool)         )
    (<              ((arg num (arg num (arglist num))) ->* bool)         )
    (>              ((arg num (arg num (arglist num))) ->* bool)         )
    (<=             ((arg num (arg num (arglist num))) ->* bool)         )
    (>=             ((arg num (arg num (arglist num))) ->* bool)         )
    (zero?          (num -> bool)                                        (T))
    (positive?      (num -> bool)                                        (T))
    (negative?      (num -> bool)                                        (T))
    (odd?           (num -> bool)                                        (T))
    (even?          (num -> bool)                                        (T))
    (max            ((arg num (arglist num)) ->* num)                    )
    (min            ((arg num (arglist num)) ->* num)                    )
    (+              ((arglist num) ->* num)                              )
    (*              ((arglist num) ->* num)                              )
    (-              ((arg num (arglist num)) ->* num)                    )
    (/              ((arg num (arglist num)) ->* num)                    )
    (abs            (num -> num)                                         (T))
    (quotient       (num num -> num)                                     (T))
    (remainder      (num num -> num)                                     (T))
    (modulo         (num num -> num)                                     (T))
    (gcd            ((arglist num) ->* num)                              )
    (lcm            ((arglist num) ->* num)                              )
    (numerator      (num -> num)                                         (T))
    (denominator    (num -> num)                                         (T))
    (floor          (num -> num)                                         (T))
    (ceiling        (num -> num)                                         (T))
    (truncate       (num -> num)                                         (T))
    (round          (num -> num)                                         (T))
    (rationalize    (num num -> num)                                     (T))
    (exp            (num -> num)                                         (T))
    (log            (num -> num)                                         (T))
    (sin            (num -> num)                                         (T))
    (cos            (num -> num)                                         (T))
    (tan            (num -> num)                                         (T))
    (asin           (num -> num)                                         (T))
    (acos           (num -> num)                                         (T))
    (atan           ((arg num (+ noarg (arg num noarg))) ->* num)        (T))
    (sqrt           (num -> num)                                         (T))
    (expt           (num num -> num)                                     (T))
    (make-rectangular (num num -> num)                                   (T))
    (make-polar     (num num -> num)                                     (T))
    (real-part      (num -> num)                                         (T))
    (imag-part      (num -> num)                                         (T))
    (magnitude      (num -> num)                                         (T))
    (angle          (num -> num)                                         (T))
    (exact->inexact (num -> num)                                         (T))
    (inexact->exact (num -> num)                                         (T))
    (number->string ((arg num (+ noarg (arg num noarg))) ->* str)        (T))
    (string->number ((arg str (+ noarg (arg num noarg))) ->* num)        (T))

    ; characters
    (char=?         (char char -> bool)                                  (T))
    (char<?         (char char -> bool)                                  (T))
    (char>?         (char char -> bool)                                  (T))
    (char<=?        (char char -> bool)                                  (T))
    (char>=?        (char char -> bool)                                  (T))
    (char-ci=?      (char char -> bool)                                  (T))
    (char-ci<?      (char char -> bool)                                  (T))
    (char-ci>?      (char char -> bool)                                  (T))
    (char-ci<=?     (char char -> bool)                                  (T))
    (char-ci>=?     (char char -> bool)                                  (T))
    (char-alphabetic? (char -> bool)                                     (T))
    (char-numeric?  (char -> bool)                                       (T))
    (char-whitespace? (char -> bool)                                     (T))
    (char-upper-case? (char -> bool)                                     (T))
    (char-lower-case? (char -> bool)                                     (T))
    (char->integer  (char -> num)                                        (T))
    (integer->char  (num -> char)                                        (T))
    (char-upcase    (char -> char)                                       (T))
    (char-downcase  (char -> char)                                       (T))

    ; strings
    (make-string    ((arg num (+ noarg (arg char noarg))) ->* str)       (T))
    (string         ((arglist char) ->* str)                             )
    (string-length  (str -> num)                                         (T))
    (string-ref     (str num -> char)                                    (T))
    (string-set!    (str num char -> void)                               (T))
    (string=?       (str str -> bool)                                    (T))
    (string<?       (str str -> bool)                                    (T))
    (string>?       (str str -> bool)                                    (T))
    (string<=?      (str str -> bool)                                    (T))
    (string>=?      (str str -> bool)                                    (T))
    (string-ci=?    (str str -> bool)                                    (T))
    (string-ci<?    (str str -> bool)                                    (T))
    (string-ci>?    (str str -> bool)                                    (T))
    (string-ci<=?   (str str -> bool)                                    (T))
    (string-ci>=?   (str str -> bool)                                    (T))
    (substring      (str num num -> str)                                 (T))
    (string-append  ((arglist str) ->* str)                              )
    (string->list   (str -> (list char))                                     (Ic))
    (list->string   ((list char) -> str)                                 )
    (string-copy    (str -> str)                                         (T))
    (string-fill!   (str char -> void)                                   (T))

    ; vectors
    (make-vector    (num a -> (vec a))                                   (T) (I)) ; different semantics
    (vector         ((arglist a) ->* (vec a))                                (I))
    (vector-length  ((vec a) -> num)                                     (T))
    (vector-ref     ((vec a) num -> a)                                   (T))
    (vector-set!    ((vec a) num a -> void)                              (T))
    (vector->list   ((vec a) -> (list a))                                    (Ic))
    (list->vector   ((list a) -> (vec a))                                    (I))
    (vector-fill!   ((vec a) a -> void)                                  (T))

    ; control features
    (apply          (((arglist a) ->* b) (list a) -> b)                      (I) (D))
    (map            ((a -> b) (list a) -> (list b))                          (I) (D))
    (for-each       ((a -> b) (list a) -> void)                              (I) (D))
    (force          ((promise a) -> a)                                   (T) (I))
    (call-with-current-continuation (((a -> b) -> a) -> a)               (T) (I))

    ; input and output
    (call-with-input-file  (str (iport -> a) -> a)                       (T) (I))
    (call-with-output-file (str (oport -> a) -> a)                       (T) (I))
    (input-port?           (a -> bool)                                   (T))
    (output-port?          (a -> bool)                                   (T))
    (current-input-port    (-> iport)                                    (T))
    (current-output-port   (-> oport)                                    (T))
    (with-input-from-file  (str (-> a) -> a)                             (T) (I))
    (with-output-to-file   (str (-> a) -> a)                             (T) (I))
    (open-input-file       (str -> iport)                                (T))
    (open-output-file      (str -> oport)                                (T))
    (close-input-port      (iport -> void)                               (T))
    (close-output-port     (oport -> void)                               (T))
    ; this type takes advantage of wrong scope of MU to get more sharing
    (read                  ((+ noarg (arg iport noarg))
                            ->*
                            (+ eof
                               num nil false true char sym str
                               (box (MU sexp
                                        (+ num nil false true char sym str
                                           (vec sexp)
                                           (cons sexp sexp)
                                           (box sexp))))
                               (cons sexp sexp)
                               (vec sexp)))                                    (I))
    (read-char             ((+ noarg (arg iport noarg)) ->* (+ char eof))  (T) (I))
    (peek-char             ((+ noarg (arg iport noarg)) ->* (+ char eof))  (T) (I))
    (char-ready?           ((+ noarg (arg iport noarg)) ->* bool)          (T) (I))
    (write                 ((arg a (+ noarg (arg oport noarg))) ->* void)  (T) (I))
    (display               ((arg a (+ noarg (arg oport noarg))) ->* void)  (T) (I))
    (newline               ((+ noarg (arg oport noarg)) ->* void)          (T) (I))
    (write-char            ((arg char (+ noarg (arg oport noarg))) ->* void)(T) (I))

    ; system interface
    (load           (str -> void)                                          (T))
    (transcript-on  (str -> void)                                          (T))
    (transcript-off (-> void)                                              (T))

    ; non R4RS extensions

    ; misc
    (symbol-append  (a ->* sym)                                            (T))
    (box            (a -> (box a))                                         (T) (I))
    (unbox          ((box a) -> a)                                         (T) (S boxX))
    (set-box!       ((box a) a -> void)                                    (T))
    (void           (-> void)                                              (T))
    (make-module    (a -> (module a))                                      (T))
    (raise          (a ->* b)                                              (T))

    ; for match
    (match:error    ((arg a (+ noarg b)) ->* c)                            (T))

    ; constant vectors
    (make-cvector   (num a -> (cvec a))                                    (T))
    (cvector        ((arglist a) ->* (cvec a))                             )
    (cvector-length ((cvec a) -> num)                                      (T))
    (cvector-ref    ((cvec a) num -> a)                                    (T))
    (cvector->list  ((cvec a) -> (list a))                                     (Ic))
    (list->cvector  ((list a) -> (cvec a))                                 )

    ; for quoted lists
    (,Qcons         (a b -> (cons a b))                                    (T) (Ic) (N))
    (,Qvector       ((arglist a) ->* (vec a))                                  (I) (N))
    (,Qbox          (a -> (box a))                                         (T) (I) (N))
    (,Qlist         ((arglist a) ->* (list a))                                 (Ic) (N))

    ; predicates
    (number?        ((+ num x) -> bool)                                  (T) (P (num)))
    (null?          ((+ nil x) -> bool)                                  (T) (P (nil)))
    (char?          ((+ char x) -> bool)                                 (T) (P (char)))
    (symbol?        ((+ sym x) -> bool)                                  (T) (P (sym)))
    (string?        ((+ str x) -> bool)                                  (T) (P (str)))
    (vector?        ((+ (vec a) x) -> bool)                              (T) (P (vec a)))
    (cvector?       ((+ (cvec a) x) -> bool)                             (T) (P (cvec a)))
    (box?           ((+ (box a) x) -> bool)                              (T) (P (box a)))
    (pair?          ((+ (cons a b) x) -> bool)                           (T) (P (cons a b)))
    (procedure?     ((+ (a ->* b) x) -> bool)                            (T) (P (->* a b)))
    (eof-object?    ((+ eof x) -> bool)                                  (T) (P (eof)))
    (input-port?    ((+ iport x) -> bool)                                (T) (P (iport)))
    (output-port?   ((+ oport x) -> bool)                                (T) (P (oport)))
    (true-object?   ((+ true x) -> bool)                                 (T) (P (true)))  ; not R4RS
    (false-object?  ((+ false x) -> bool)                                (T) (P (false))) ; not R4RS
    (module?        ((+ (module a) x) -> bool)                           (T) (P (module a))) ; not R4RS
    ;; These last two are not disjoint predicates.
    ;; improve-clauses checks for them explicitly.
    (boolean?       ((+ true false x) -> bool)                           (T) (P #t))
    (list?          ((MU u (+ nil (cons y u) x)) -> bool)                (T) (P #t))
    ))

(define initial-env ())

(define init-env!
  (lambda ()
    (set! initial-env
      (foldr
        (lambda (l env)
          (letrec ((build-selector
                    (match-lambda
                      ('X
                        (lambda (X) X))
                      ('_
                        (lambda (X) (make-Pany)))
                      ('boxX
                        (let ((c (lookup env 'box?)))
                          (lambda (X)
                            (make-Pobj c (list X)))))
                      ((x . y)
                       (let ((c (lookup env 'pair?))
                             (lx (build-selector x))
                             (ly (build-selector y)))
                         (lambda (X)
                           (make-Pobj c (list (lx X) (ly X)))))))))
            (match l
              ((name type . attr)
               (let* ((pure (cond ((assq 'I attr) #f)
                                  ((assq 'Ic attr) 'cons)
                                  (else #t)))
                      (def (assq 'D attr))
                      (check (assq 'C attr))
                      (nocheck (assq 'N attr))
                      (subt (and (assq 'T attr) #t))
                      (pred (match (assq 'P attr)
                              (#f #f)
                              ((_ #t) #t)
                              ((_ (tag . args)) (cons (lookup initial-type-env tag) args))))
                      (sel (match (assq 'S attr)
                             (#f #f)
                             ((_ s) (build-selector s))))
                      (env1 (extend-env env name
                              (make-Name
                                name
                                (closeall (R+ ord-depth initial-type-env type))
                                0
                                #f
                                #f
                                (cond (nocheck 'nocheck)(check 'check)(def 'imprecise)(else #t))
                                #f
                                pure
                                pred
                                #f
                                sel
                                subt)))
                      (env2 (extend-env env1 (symbol-append 'CHECK- name)
                              (make-Name
                                (symbol-append 'CHECK- name)
                                (closeall (R++ ord-depth initial-type-env type))
                                0
                                #f
                                #f
                                #t
                                #f
                                pure
                                pred
                                #f
                                sel
                                subt))))
                 env2)))))
        empty-env
        initial-info))))

(define defprim
  (lambda (name type mode)
    (handle
      (R+ ord-depth initial-type-env type)
      (match-lambda*
        (('type . args) (apply syntax-error type args))
        (x (apply raise x))))
    (let* ((attr (match mode
                   ('impure '((I)))
                   ('pure ())
                   ('pure-if-cons-is '((Ic)))
                   ('mutates-cons
                     (set! cons-mutators (cons name cons-mutators))
                     '())
                   (x (use-error "invalid attribute ~a for define-primitive" x))))
           (info `(,name ,type ,@attr)))
      (unless (equal? info (assq name initial-info))
        (set! initial-info (cons info initial-info))
        (set! init-env-mangled #t)))))

(define init-env-mangled #f)

(define reinit-env!
  (lambda ()
    (when init-env-mangled
      (reinit-types!)
      (init-env!)
      (set! init-env-mangled #f))))
