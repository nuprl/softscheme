;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System specific functions.  SLIB version.

(if (equal? '(match 1) (macroexpand-1 '(match 1)))
    (load "/home/wright/scheme/match/match-slib.scm"))

(require 'pretty-print)
(require 'format)
(require 'sort)

(define sprintf (lambda args (apply format #f args)))
(define printf  (lambda args (apply format #t args)))

(define disaster
  (lambda (context fmt . args)
    (slib:error (apply sprintf (string-append "in ~a: " fmt) context args))))

(define use-error
  (lambda (fmt . args)
    (slib:error (apply sprintf fmt args))))

(define syntax-error
  (lambda (context fmt . args)
    (newline)
    (if context (pretty-print context))
    (slib:error (apply sprintf (string-append "in syntax: " fmt) args))))

(define flush-output force-output)

(define print-context
  (lambda (obj depth)
    (pretty-print
      (recur loop ((obj obj)(n 0))
        (if (pair? obj)
            (if (< n depth)
                (cons (loop (car obj) (+ 1 n)) (loop (cdr obj) n))
                '(...))
            obj)))))

;; Boxes, but not real safe cause they look like pairs.
(define *box-tag* (gensym))
(define box (lambda (a) (cons *box-tag* a)))
(define box? (lambda (b) (and (pair? b) (eq? (car b) *box-tag*))))
(define unbox cdr)
(define box-1 cdr)
(define set-box! set-cdr!)

(define sort-list sort)

(define expand-once macroexpand-1)

(define andmap
  (lambda (f . lists)
    (cond ((null? (car lists)) (and))
          ((null? (cdr (car lists))) (apply f (map car lists)))
          (else (and (apply f (map car lists))
                     (apply andmap f (map cdr lists)))))))

(define ormap
  (lambda (f . lists)
    (if (null? (car lists))
        (or)
        (or (apply f (map car lists))
            (apply ormap f (map cdr lists))))))

(define call/cc call-with-current-continuation)

(define fx+ +)
(define fx- -)
