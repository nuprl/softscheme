;; A simple tautology checker.  A tautology is:
;;   #t
;;   a procedure that returns a tautology whether given #t or #f.

(define taut
  (match-lambda
    (#t #t)
    (#f #f)
    ((? procedure? f) (and (taut (f #t)) (taut (f #f))))))

;; map from Left to Right.

(define mapLR
  (lambda (f l)
    (if (null? l)
        ()
        (let ((v (f (car l))))
          (cons v (mapLR f (cdr l)))))))

(define ok
  (mapLR taut
    `(#t #f ,not ,(lambda (x) #t) ,(lambda (x) (lambda (y) #t)))))
(display ok)
(newline)

(define notok (mapLR taut '(#t #f . 3)))
(display "you won't see this as the argument to mapLR is not a list")

(define wrong (+ 1 "hi"))  ; an error definitely occurs if we ever get here.
