;; requires expand-once, error, and gensym

(defmacro encapsulate args
  (letrec
    ((expand-to-defines
      (lambda (def)
        (match def
          (('define (? symbol? n) e)
           (list def))
          (('define ((? symbol? n) . args) . body)
           (list `(define ,n (lambda ,args ,@body))))
          (('begin . (? list? defs))
           (foldr append () (map expand-to-defines defs)))
          (((? symbol? k) . _)
           (=> fail)
           (let ((d2 (expand-once def)))
             (if (equal? def d2)
                 (fail)
                 (expand-to-defines d2))))
          (_ (list `(define ,(gensym) ,def))))))
     (readfile
      (lambda (f)
        (with-input-from-file f
          (letrec ((rf (lambda ()
                         (match (read)
                           ((? eof-object?) ())
                           (sexp (cons sexp (rf)))))))
            rf))))
     (foldr
      (lambda (f i l)
        (recur loop ((l l))
          (match l
            (() i)
            ((x . y) (f x (loop y))))))))
    (match args
      ((((? symbol? exports) ...) files ...)
       (let* ((pgm `(begin ,@(map (lambda (f) `(begin ,@(readfile f))) (map eval files))))
              (defs (expand-to-defines pgm)))
         `(match-define ,exports
            (let ,(map (match-lambda (('define x _) `(,x (void))))
                     defs)
              ,@(map (match-lambda (('define x e) `(set! ,x ,e)))
                     defs)
              (list ,@exports)))))
      (_ (error 'encapsulate "invalid syntax")))))
