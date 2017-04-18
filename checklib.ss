;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Things for running soft typed code.

(defmacro CHECK-INCREMENT-COUNTER args
  #f)

(defmacro CLASH args
  (match args
    ((name info ...) name)))

(defmacro CHECK-lambda args
  (match args
    (((id info ...) (? symbol? args) body ..1)
     `(lambda ,args
        (CHECK-INCREMENT-COUNTER ,id)
        ,@body))
    (((id info ...) args body ..1)
     (let* ((g (gensym))
            (n 0)
            (chk (let loop ((a args)(nargs 0))
                   (cond ((pair? a)
                          (loop (cdr a) (+ 1 nargs)))
                         ((null? a)
                          (set! n nargs)
                          `(= ,nargs (length ,g)))
                         (else
                          (set! n nargs)
                          `(<= ,nargs (length ,g))))))
            (incr (if (number? id) `(CHECK-INCREMENT-COUNTER ,id) #f)))
       `(lambda ,g
          ,incr
          (if ,chk
              (apply (lambda ,args ,@body) ,g)
              ,(if (eq? '= (car chk))
                   `(st:failure '(CHECK-lambda ,id ,@info)
                      "requires ~a arguments, passed: ~a" ,n ,g)
                   `(st:failure '(CHECK-lambda ,id ,@info)
                      "requires >= ~a arguments, passed: ~a" ,n ,g))))))))

(defmacro CHECK-ap args
  (match args
    (((id info ...) (? symbol? f) args ...)
     `(begin
        (CHECK-INCREMENT-COUNTER ,id)
        (if (procedure? ,f)
            (,f ,@args)
            (st:failure '(CHECK-ap ,id ,@info) "not a procedure: ~a" ,f))))
    (((id info ...) f args ...)
     (let ((g (gensym)))
       `(let ((,g ,f))
          (CHECK-INCREMENT-COUNTER ,id)
          (if (procedure? ,g)
              (,g ,@args)
              (st:failure '(CHECK-ap ,id ,@info) "not a procedure: ~a" ,g)))))))

(defmacro CHECK-field args
  (match args
    (((id info ...) (? symbol? f) exp)
     `(match ,exp
        (($ record x) (match (assq ',f x)
                        (#f (st:failure '(CHECK-field ,id ,@info)
                              "no ~a field in (record ~a)" ',f (map car x)))
                        ((_ . x) x)))
        (v (st:failure '(CHECK-field ,id ,@info) "not a record: ~a" v))))))

(defmacro CHECK-match args
  (match args
    (((id info ...) exp clause ...)
     `(begin
        (CHECK-INCREMENT-COUNTER ,id)
        (match ,exp
          ,@clause
          (x (st:failure '(CHECK-match ,id ,@info) "no matching clause for ~a" x)))))))

(defmacro CHECK-: args
  (match args
    (((id info ...) typeexp exp)
     `(st:failure '(CHECK-: ,id ,@info) "static type annotation reached"))))

(defmacro make-CHECK-onearg args
  (match args
    ((prim)
     (let ((chkprim (symbol-append 'CHECK- prim)))
       `(define ,chkprim
          (lambda id
            (lambda a
              (CHECK-INCREMENT-COUNTER (car id))
              (if (= 1 (length a))
                  (,prim (car a))
                  (st:failure (cons ',chkprim id) "invalid arguments: ~a" a)))))))))

(defmacro make-CHECK-twoarg args
  (match args
    ((prim)
     (let ((chkprim (symbol-append 'CHECK- prim)))
       `(define ,chkprim
          (lambda id
            (lambda a
              (CHECK-INCREMENT-COUNTER (car id))
              (if (= 2 (length a))
                  (,prim (car a) (cadr a))
                  (st:failure (cons ',chkprim id) "invalid arguments: ~a" a)))))))))

(defmacro make-CHECK-typed args
  (match args
    ((prim)
     (let ((chkprim (symbol-append 'CHECK- prim)))
       `(define ,chkprim
          (lambda id
            (lambda a
              (CHECK-INCREMENT-COUNTER (car id))
              (if (null? a)
                  (,prim)
                  (st:failure (cons ',chkprim id) "invalid arguments: ~a" a)))))))
    ((prim type1)
     (let ((chkprim (symbol-append 'CHECK- prim)))
       `(define ,chkprim
          (lambda id
            (lambda a
              (CHECK-INCREMENT-COUNTER (car id))
              (if (and (= 1 (length a))
                       (,type1 (car a)))
                  (,prim (car a))
                  (st:failure (cons ',chkprim id) "invalid arguments: ~a" a)))))))
    ((prim type1 type2)
     (let ((chkprim (symbol-append 'CHECK- prim)))
       `(define ,chkprim
          (lambda id
            (lambda a
              (CHECK-INCREMENT-COUNTER (car id))
              (if (and (= 2 (length a))
                       (,type1 (car a))
                       (,type2 (cadr a)))
                  (,prim (car a) (cadr a))
                  (st:failure (cons ',chkprim id) "invalid arguments: ~a" a)))))))
    ((prim types ...)
     (let ((nargs (length types))
           (chkprim (symbol-append 'CHECK- prim)))
       `(define ,chkprim
          (lambda id
            (lambda a
              (CHECK-INCREMENT-COUNTER (car id))
              (if (and (= ,nargs (length a))
                       (andmap (lambda (f a) (f a)) (list ,@types) a))
                  (apply ,prim a)
                  (st:failure (cons ',chkprim id) "invalid arguments: ~a" a)))))))))

(defmacro make-CHECK-selector args
  (match args
    ((prim pat)
     (let ((chkprim (symbol-append 'CHECK- prim)))
       `(define ,chkprim
          (lambda id
            (lambda a
              (CHECK-INCREMENT-COUNTER (car id))
              (match a
                ((,pat) X)
                (_ (st:failure (cons ',chkprim id) "invalid arguments: ~a" a))))))))))

    ; predicates
(make-CHECK-onearg number?)
(make-CHECK-onearg null?)
(make-CHECK-onearg char?)
(make-CHECK-onearg symbol?)
(make-CHECK-onearg string?)
(make-CHECK-onearg vector?)
(make-CHECK-onearg box?)
(make-CHECK-onearg pair?)
(make-CHECK-onearg procedure?)
(make-CHECK-onearg eof-object?)
(make-CHECK-onearg input-port?)
(make-CHECK-onearg output-port?)
(make-CHECK-onearg true-object?)
(make-CHECK-onearg false-object?)
(make-CHECK-onearg boolean?)
(make-CHECK-onearg list?)

    ; booleans
(make-CHECK-onearg not)

    ; equivalence predicates
(make-CHECK-twoarg eqv?)
(make-CHECK-twoarg eq?)
(make-CHECK-twoarg equal?)

    ; pairs and lists
(make-CHECK-twoarg cons)
(make-CHECK-selector car (X . _))
(make-CHECK-selector cdr (_ . X))
(make-CHECK-selector caar ((X . _) . _))
(make-CHECK-selector cadr (_ X . _))
(make-CHECK-selector cdar ((_ . X) . _))
(make-CHECK-selector cddr (_ _ . X))
(make-CHECK-selector caaar (((X . _) . _) . _))
(make-CHECK-selector caadr (_ (X . _) . _))
(make-CHECK-selector cadar ((_ X . _) . _))
(make-CHECK-selector caddr (_ _ X . _))
(make-CHECK-selector cdaar (((_ . X) . _) . _))
(make-CHECK-selector cdadr (_ (_ . X) . _))
(make-CHECK-selector cddar ((_ _ . X) . _))
(make-CHECK-selector cdddr (_ _ _ . X))
(make-CHECK-selector caaaar ((((X . _) . _) . _) . _))
(make-CHECK-selector caaadr (_ ((X . _) . _) . _))
(make-CHECK-selector caadar ((_ (X . _) . _) . _))
(make-CHECK-selector caaddr (_ _ (X . _) . _))
(make-CHECK-selector cadaar (((_ X . _) . _) . _))
(make-CHECK-selector cadadr (_ (_ X . _) . _))
(make-CHECK-selector caddar ((_ _ X . _) . _))
(make-CHECK-selector cadddr (_ _ _ X . _))
(make-CHECK-selector cdaaar ((((_ . X) . _) . _) . _))
(make-CHECK-selector cdaadr (_ ((_ . X) . _) . _))
(make-CHECK-selector cdadar ((_ (_ . X) . _) . _))
(make-CHECK-selector cdaddr (_ _ (_ . X) . _))
(make-CHECK-selector cddaar (((_ _ . X) . _) . _))
(make-CHECK-selector cddadr (_ (_ _ . X) . _))
(make-CHECK-selector cdddar ((_ _ _ . X) . _))
(make-CHECK-selector cddddr (_ _ _ _ . X))
(make-CHECK-typed set-car! pair? (lambda (_) #t))
(make-CHECK-typed set-cdr! pair? (lambda (_) #t))
(define CHECK-list
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (apply list a))))
(make-CHECK-typed length list?)
(define CHECK-append
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (let loop ((b a))
        (match b
          (() #t)
          ((l) #t)
          (((? list?) . y) (loop y))
          (_ (st:failure `(CHECK-append ,@id) "invalid arguments: ~a" a))))
      (apply append a))))
(make-CHECK-typed reverse list?)
(make-CHECK-typed list-tail list? number?)
(make-CHECK-typed list-ref list? number?)
(make-CHECK-typed memq (lambda (_) #t) list?)
(make-CHECK-typed memv (lambda (_) #t) list?)
(make-CHECK-typed member (lambda (_) #t) list?)
(define CHECK-assq
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (= 2 (length a))
               (list? (cadr a))
               (andmap pair? (cadr a)))
          (assq (car a) (cadr a))
          (st:failure `(CHECK-assq ,@id) "invalid arguments: ~a" a)))))
(define CHECK-assv
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (= 2 (length a))
               (list? (cadr a))
               (andmap pair? (cadr a)))
          (assv (car a) (cadr a))
          (st:failure `(CHECK-assv ,@id) "invalid arguments: ~a" a)))))
(define CHECK-assoc
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (= 2 (length a))
               (list? (cadr a))
               (andmap pair? (cadr a)))
          (assoc (car a) (cadr a))
          (st:failure `(CHECK-assoc ,@id) "invalid arguments: ~a" a)))))

    ; symbols
(make-CHECK-typed symbol->string symbol?)
(make-CHECK-typed string->symbol string?)

    ; numbers
(make-CHECK-onearg complex?)
(make-CHECK-onearg real?)
(make-CHECK-onearg rational?)
(make-CHECK-onearg integer?)
(make-CHECK-typed exact? number?)
(make-CHECK-typed inexact? number?)
(define CHECK-=
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (<= 2 (length a))
               (andmap number? a))
          (apply = a)
          (st:failure `(CHECK-= ,@id) "invalid arguments: ~a" a)))))
(define CHECK-<
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (<= 2 (length a))
               (andmap number? a))
          (apply < a)
          (st:failure `(CHECK-< ,@id) "invalid arguments: ~a" a)))))
(define CHECK->
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (<= 2 (length a))
               (andmap number? a))
          (apply > a)
          (st:failure `(CHECK-> ,@id) "invalid arguments: ~a" a)))))
(define CHECK-<=
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (<= 2 (length a))
               (andmap number? a))
          (apply <= a)
          (st:failure `(CHECK-<= ,@id) "invalid arguments: ~a" a)))))
(define CHECK->=
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (<= 2 (length a))
               (andmap number? a))
          (apply >= a)
          (st:failure `(CHECK->= ,@id) "invalid arguments: ~a" a)))))
(make-CHECK-typed zero? number?)
(make-CHECK-typed positive? number?)
(make-CHECK-typed negative? number?)
(make-CHECK-typed odd? number?)
(make-CHECK-typed even? number?)
(define CHECK-max
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (<= 1 (length a))
               (andmap number? a))
          (apply max a)
          (st:failure `(CHECK-max ,@id) "invalid arguments: ~a" a)))))
(define CHECK-min
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (<= 1 (length a))
               (andmap number? a))
          (apply min a)
          (st:failure `(CHECK-min ,@id) "invalid arguments: ~a" a)))))
(define CHECK-+
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (andmap number? a)
          (apply + a)
          (st:failure `(CHECK-+ ,@id) "invalid arguments: ~a" a)))))
(define CHECK-*
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (andmap number? a)
          (apply * a)
          (st:failure `(CHECK-* ,@id) "invalid arguments: ~a" a)))))
(define CHECK--
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (<= 1 (length a))
               (andmap number? a))
          (apply - a)
          (st:failure `(CHECK-- ,@id) "invalid arguments: ~a" a)))))
(define CHECK-/
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (<= 1 (length a))
               (andmap number? a))
          (apply / a)
          (st:failure `(CHECK-/ ,@id) "invalid arguments: ~a" a)))))
(make-CHECK-typed abs number?)
(make-CHECK-typed quotient number? number?)
(make-CHECK-typed remainder number? number?)
(make-CHECK-typed modulo number? number?)
(define CHECK-gcd
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (andmap number? a)
          (apply gcd a)
          (st:failure `(CHECK-gcd ,@id) "invalid arguments: ~a" a)))))
(define CHECK-lcm
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (andmap number? a)
          (apply lcm a)
          (st:failure `(CHECK-lcm ,@id) "invalid arguments: ~a" a)))))
(make-CHECK-typed numerator number?)
(make-CHECK-typed denominator number?)
(make-CHECK-typed floor number?)
(make-CHECK-typed ceiling number?)
(make-CHECK-typed truncate number?)
(make-CHECK-typed round number?)
(make-CHECK-typed rationalize number? number?)
(make-CHECK-typed exp number?)
(make-CHECK-typed log number?)
(make-CHECK-typed sin number?)
(make-CHECK-typed cos number?)
(make-CHECK-typed tan number?)
(make-CHECK-typed asin number?)
(make-CHECK-typed acos number?)
(define CHECK-atan
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (andmap number? a) (pair? a) (>= 2 (length a)))
          (apply atan a)
          (st:failure `(CHECK-atan ,@id) "invalid arguments: ~a" a)))))
(make-CHECK-typed sqrt number?)
(make-CHECK-typed expt number? number?)
(make-CHECK-typed make-rectangular number? number?)
(make-CHECK-typed make-polar number? number?)
(make-CHECK-typed real-part number?)
(make-CHECK-typed imag-part number?)
(make-CHECK-typed magnitude number?)
(make-CHECK-typed angle number?)
(make-CHECK-typed exact->inexact number?)
(make-CHECK-typed inexact->exact number?)
(define CHECK-number->string
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (andmap number? a) (pair? a) (>= 2 (length a)))
          (apply number->string a)
          (st:failure `(CHECK-number->string ,@id) "invalid arguments: ~a" a)))))
(define CHECK-string->number
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (pair? a) (string? (car a)) (>= 2 (length a))
               (or (null? (cdr a)) (number? (cadr a))))
          (apply string->number a)
          (st:failure `(CHECK-string->number ,@id) "invalid arguments: ~a" a)))))

    ; characters
(make-CHECK-typed char=? char? char?)
(make-CHECK-typed char<? char? char?)
(make-CHECK-typed char>? char? char?)
(make-CHECK-typed char<=? char? char?)
(make-CHECK-typed char>=? char? char?)
(make-CHECK-typed char-ci=? char? char?)
(make-CHECK-typed char-ci<? char? char?)
(make-CHECK-typed char-ci>? char? char?)
(make-CHECK-typed char-ci<=? char? char?)
(make-CHECK-typed char-ci>=? char? char?)
(make-CHECK-typed char-alphabetic? char?)
(make-CHECK-typed char-numeric? char?)
(make-CHECK-typed char-whitespace? char?)
(make-CHECK-typed char-upper-case? char?)
(make-CHECK-typed char-lower-case? char?)
(make-CHECK-typed char->integer char?)
(make-CHECK-typed integer->char number?)
(make-CHECK-typed char-upcase char?)
(make-CHECK-typed char-downcase char?)

    ; strings
(define CHECK-make-string
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (pair? a) (number? (car a)) (>= 2 (length a))
               (or (null? (cdr a)) (char? (cadr a))))
          (apply make-string a)
          (st:failure `(CHECK-make-string ,@id) "invalid arguments: ~a" a)))))
(define CHECK-string
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (andmap char? a)
          (apply string a)
          (st:failure `(CHECK-string ,@id) "invalid arguments: ~a" a)))))
(make-CHECK-typed string-length string?)
(make-CHECK-typed string-ref string? number?)
(make-CHECK-typed string-set! string? number? char?)
(make-CHECK-typed string=? string? string?)
(make-CHECK-typed string<? string? string?)
(make-CHECK-typed string>? string? string?)
(make-CHECK-typed string<=? string? string?)
(make-CHECK-typed string>=? string? string?)
(make-CHECK-typed string-ci=? string? string?)
(make-CHECK-typed string-ci<? string? string?)
(make-CHECK-typed string-ci>? string? string?)
(make-CHECK-typed string-ci<=? string? string?)
(make-CHECK-typed string-ci>=? string? string?)
(make-CHECK-typed substring string? number? number?)
(define CHECK-string-append
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (andmap string? a)
          (apply string-append a)
          (st:failure `(CHECK-string-append ,@id) "invalid arguments: ~a" a)))))
(make-CHECK-typed string->list string?)
(define CHECK-list->string
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (= 1 (length a)) (list? (car a)) (andmap char? (car a)))
          (list->string (car a))
          (st:failure `(CHECK-list->string ,@id) "invalid arguments: ~a" a)))))
(make-CHECK-typed string-copy string?)
(make-CHECK-typed string-fill! string? char?)

    ; vectors
(make-CHECK-typed make-vector number? (lambda (_) #t))
(define CHECK-vector
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (apply vector a))))
(make-CHECK-typed vector-length vector?)
(make-CHECK-typed vector-ref vector? number?)
(make-CHECK-typed vector-set! vector? number? (lambda (_) #t))
(make-CHECK-typed vector->list vector?)
(make-CHECK-typed list->vector list?)
(make-CHECK-typed vector-fill! vector? (lambda (_) #t))

    ; control features
(define CHECK-apply
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (pair? a)
          (let loop ((arg (cdr a)))
            (match arg
              (((? list?)) (apply apply a))
              ((_ . y) (loop y))
              (_ (st:failure `(CHECK-apply ,@id) "invalid arguments: ~a" a))))
          (st:failure `(CHECK-apply ,@id) "invalid arguments: ~a" a)))))
(define CHECK-map
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (<= 2 (length a)) (procedure? (car a)) (andmap list? (cdr a)))
          (apply map a)
          (st:failure `(CHECK-map ,@id) "invalid arguments: ~a" a)))))
(define CHECK-for-each
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (<= 2 (length a)) (procedure? (car a)) (andmap list? (cdr a)))
          (apply for-each a)
          (st:failure `(CHECK-for-each ,@id) "invalid arguments: ~a" a)))))
(make-CHECK-typed force procedure?)
(define CHECK-call-with-current-continuation
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (= 1 (length a)) (procedure? (car a)))
          (call-with-current-continuation
            (lambda (k) ((car a) (CHECK-lambda (continuation) (x) (k x)))))
          (st:failure `(CHECK-call-with-current-continuation ,@id)
            "invalid arguments: ~a" a)))))

    ; input and output
(make-CHECK-typed call-with-input-file string? procedure?)
(make-CHECK-typed call-with-output-file string? procedure?)
(make-CHECK-onearg input-port?)
(make-CHECK-onearg output-port?)
(make-CHECK-typed current-input-port)
(make-CHECK-typed current-output-port)
(make-CHECK-typed with-input-from-file string? procedure?)
(make-CHECK-typed with-output-to-file string? procedure?)
(make-CHECK-typed open-input-file string?)
(make-CHECK-typed open-output-file string?)
(make-CHECK-typed close-input-port input-port?)
(make-CHECK-typed close-output-port output-port?)
(define CHECK-read
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (or (null? a) (and (= 1 (length a)) (input-port? (car a))))
          (apply read a)
          (st:failure `(CHECK-read ,@id) "invalid arguments: ~a" a)))))
(define CHECK-read-char
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (or (null? a) (and (= 1 (length a)) (input-port? (car a))))
          (apply read-char a)
          (st:failure `(CHECK-read-char ,@id) "invalid arguments: ~a" a)))))
(define CHECK-peek-char
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (or (null? a) (and (= 1 (length a)) (input-port? (car a))))
          (apply peek-char a)
          (st:failure `(CHECK-peek-char ,@id) "invalid arguments: ~a" a)))))
(define CHECK-char-ready?
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (or (null? a) (and (= 1 (length a)) (input-port? (car a))))
          (apply char-ready? a)
          (st:failure `(CHECK-char-ready? ,@id) "invalid arguments: ~a" a)))))
(define CHECK-write
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (pair? a) (or (null? (cdr a)) (output-port? (cadr a))))
          (apply write a)
          (st:failure `(CHECK-write ,@id) "invalid arguments: ~a" a)))))
(define CHECK-display
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (pair? a) (or (null? (cdr a)) (output-port? (cadr a))))
          (apply display a)
          (st:failure `(CHECK-display ,@id) "invalid arguments: ~a" a)))))
(define CHECK-newline
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (or (null? a) (output-port? (car a)))
          (apply newline a)
          (st:failure `(CHECK-newline ,@id) "invalid arguments: ~a" a)))))
(define CHECK-write-char
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (pair? a) (char? (car a))
               (or (null? (cdr a)) (output-port? (cadr a))))
          (apply write-char a)
          (st:failure `(CHECK-write-char ,@id) "invalid arguments: ~a" a)))))

    ; system interface
(make-CHECK-typed load string?)
(make-CHECK-typed transcript-on string?)
(make-CHECK-typed transcript-off)

    ; non-R4RS extensions

    ; misc
(define CHECK-symbol-append
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (apply symbol-append a))))
(make-CHECK-onearg box)
(make-CHECK-typed unbox box?)
(make-CHECK-typed set-box! box? (lambda (_) #t))
(make-CHECK-typed void)
(make-CHECK-onearg make-module)
; raise never needs a CHECK

    ; for match
(define CHECK-match:error
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (pair? a)
          (apply match:error a)
          (st:failure `(CHECK-match:error ,@id) "invalid arguments: ~a" a)))))

    ; constant vectors
(define CHECK-make-cvector
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (if (and (pair? a) (number? (car a)) (= 2 (length a)))
          (apply make-cvector a)
          (st:failure `(CHECK-make-cvector ,@id) "invalid arguments: ~a" a)))))
(define CHECK-cvector
  (lambda id
    (lambda a
      (CHECK-INCREMENT-COUNTER (car id))
      (apply cvector a))))
(make-CHECK-typed cvector-length cvector?)
(make-CHECK-typed cvector-ref cvector? number?)
(make-CHECK-typed cvector->list cvector?)
(make-CHECK-typed list->cvector list?)



    ; structures
(defmacro CHECK-define-const-structure args
  (let ((field? (lambda (x)
                  (or (symbol? x)
                      (and (pair? x)
                           (equal? (car x) '!)
                           (pair? (cdr x))
                           (symbol? (cadr x))
                           (null? (cddr x))))))
        (arg-name (lambda (x) (if (symbol? x) x (cadr x))))
        (with-mutator? (lambda (x) (not (symbol? x)))))
    (match args
      ((((? symbol? name) (? field? id1) ...))
       (let ((constructor
              (symbol-append 'make- name))
             (CHECK-constructor
              (symbol-append 'CHECK-make- name))
             (predicate
              (symbol-append name '?))
             (access
              (let loop ((l id1))
                (cond
                  ((null? l) ())
                  ((eq? '_ (arg-name (car l))) (loop (cdr l)))
                  (else (cons (symbol-append name '- (arg-name (car l)))
                              (loop (cdr l)))))))
             (assign
              (let loop ((l id1))
                (cond
                  ((null? l) ())
                  ((eq? '_ (arg-name (car l))) (loop (cdr l)))
                  ((not (with-mutator? (car l))) (loop (cdr l)))
                  (else
                   (cons (symbol-append 'set- name '- (arg-name (car l)) '!)
                         (loop (cdr l)))))))
             (nargs (length id1)))
         `(begin
            (define-const-structure (,name ,@id1) ())
            (define ,CHECK-constructor
              (lambda id
                (lambda a
                  (CHECK-INCREMENT-COUNTER (car id))
                  (if (= ,nargs (length a))
                      (apply ,constructor a)
                      (st:failure (cons ',CHECK-constructor id) "invalid arguments: ~a" a)))))
            (make-CHECK-onearg ,predicate)
            ,@(map (lambda (a)
                     `(make-CHECK-typed ,a ,predicate))
                   access)
            ,@(map (lambda (a)
                     `(make-CHECK-typed ,a ,predicate (lambda (_) #t)))
                   assign)
            ))))))
