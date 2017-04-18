(define cond-tf
  (lambda (cond-expr)
    (recur loop ((e (cdr cond-expr)))
      (match e
        (() `(,(primitive 'void)))
        ((('else b1 . body)) `(begin ,b1 ,@body))
        ((('else . _) . _)
         (syntax-error cond-expr "invalid cond expression"))
        (((test '=> proc) . rest)
         (let ((g (gensym)))
           `(let ((,g ,test))
              (if ,g (,proc ,g) ,(loop rest)))))
        (((#t b1 . body)) `(begin ,b1 ,@body))  ; only diff from Chez?
        (((test) . rest) `(or ,test ,(loop rest)))
        (((test . body) . rest) `(if ,test (begin ,@body) ,(loop rest)))
        (_ (syntax-error cond-expr "invalid cond expression"))))))

(define case-tf
  (lambda (case-expr)
    (recur loop ((e (cdr case-expr)))
      (match e
        ((exp) `(begin ,exp (,(primitive 'void))))
        ((exp ('else b1 . body)) `(begin ,b1 ,@body))
        ((exp ('else . _) . _)
         (syntax-error case-expr "invalid case expression"))
        (((? symbol? exp) ((? list? test) b1 . body) . rest)
         `(if (,(primitive 'memv) ,exp (quote ,test))
              (begin ,b1 ,@body)
              ,(loop (cons exp rest))))
        (((? symbol? exp) (test b1 . body) . rest)
         `(if (,(primitive 'memv) ,exp (quote (,test)))   ; Chez extension
              (begin ,b1 ,@body)
              ,(loop (cons exp rest))))
        ((exp . rest)
         (if (not (symbol? exp))
             (let ((g (gensym)))
               `(let ((,g ,exp))
                  ,(loop (cons g rest))))
             (syntax-error case-expr "invalid case expression")))
        (_ (syntax-error case-expr "invalid case expression"))))))

(define ConsLimit 8)

(define quote-tf
  (lambda (exp)
    (letrec ((qloop
               (match-lambda
                 ((? box? q) `(,(primitive Qbox) ,(qloop (unbox q))))
                 ((? symbol? q) `(quote ,q))
                 ((? null? q) q)
                 ((? list? q) (if (< (length q) ConsLimit)
                                  `(,(primitive Qcons) ,(qloop (car q)) ,(qloop (cdr q)))
                                  `(,(primitive Qlist) ,@(map qloop q))))
                 ((x . y) `(,(primitive Qcons) ,(qloop x) ,(qloop y)))
                 ((? vector? q) `(,(primitive Qvector) ,@(map qloop (vector->list q))))
                 ((? boolean? q) q)
                 ((? number? q) q)
                 ((? char? q) q)
                 ((? string? q) q)
                 (q (syntax-error exp "invalid quote expression at ~a" q)))))
      (match exp
        (('quote q) (qloop q))
        ((? vector? q) (qloop q))
        ((? box? q) (qloop q))))))

; Note that improve expands short (primitive 'list) applications.
(define quasiquote-tf
  (lambda (exp)
    (letrec ((make-cons
              (lambda (x y)
                (cond ((null? y) `(,(primitive 'list) ,x))
                      ((and (pair? y) (equal? (car y) (primitive 'list)))
                       (cons (car y) (cons x (cdr y))))
                      (else `(,(primitive 'cons) ,x ,y)))))
             (qloop
               (lambda (e n)
                 (match e
                   (('quasiquote e)
                    (make-cons 'quasiquote (qloop `(,e) (add1 n))))
                   (('unquote e)
                    (if (zero? n)
                        e
                        (make-cons 'unquote (qloop `(,e) (sub1 n)))))
                   (('unquote-splicing e)
                    (if (zero? n)
                        e
                        (make-cons 'unquote-splicing (qloop `(,e) (sub1 n)))))
                   ((('unquote-splicing e) . y)
                    (=> fail)
                    (if (zero? n)
                        (if (null? y)
                            e
                            `(,(primitive 'append) ,e ,(qloop y n)))
                        (fail)))
                   ((? box? q) `(,(primitive 'box) ,(qloop (unbox q) n)))
                   ((? symbol? q) 
                    (if (memq q '(quasiquote unquote unquote-splicing))
                        (syntax-error exp
                          "invalid use of ~a inside quasiquote" q)
                        `(quote ,q)))
                   ((? null? q) q)
                   ((x . y) (make-cons (qloop x n) (qloop y n)))
                   ((? vector? q) `(,(primitive 'vector)
                                    ,@(map (lambda (z) (qloop z n))
                                           (vector->list q))))
                   ((? boolean? q) q)
                   ((? number? q) q)
                   ((? char? q) q)
                   ((? string? q) q)
                   (q (syntax-error exp
                        "invalid quasiquote expression at ~a" q))))))
      (match exp
        (('quasiquote q) (qloop q 0))))))

(define do-tf
  (lambda (do-expr)
    (recur loop ((e (cdr do-expr)))
      (match e
        ((((var init . step) ...) (e0 e1 ...) c ...)
         (let ((step
                (map (lambda (v s)
                       (match s
                         (() v)
                         ((e) e)
                         (_ (syntax-error do-expr "invalid do expression"))))
                     var
                     step)))
           (let ((doloop (gensym)))
             (match e1
               (()
                `(let ,doloop ,(map list var init)
                   (if (not ,e0)
                       (begin ,@c (,doloop ,@step) (void))
                       (void))))
               ((body ..1)
                `(let ,doloop ,(map list var init)
                   (if ,e0
                       (begin ,@body)
                       (begin ,@c (,doloop ,@step)))))
               (_ (syntax-error do-expr "invalid do expression"))))))
        (_ (syntax-error do-expr "invalid do expression"))))))
