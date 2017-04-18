;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System specific functions.  Chez Scheme version.

(define sprintf format)

(define disaster error)

(define use-error
  (lambda (fmt . args)
    (newline)
    (apply printf (string-append "Error: " fmt) args)
    (newline)
    (reset)))

(define syntax-error
  (lambda (context format . args)
    (newline)
    (when context
      (print-context context 4))
    (apply printf (string-append "Syntax Error: " format) args)
    (newline)
    (reset)))

(define print-context
  (lambda (obj depth)
    (parameterize ((print-level depth))
      (pretty-print obj))))

(define box-1 unbox)

(define sort-list (lambda (l p) (#%sort p l)))

(define macro?
  (lambda (m)
    (and (not (eq? m '|#primitive|)) (get m '*expander*))))
