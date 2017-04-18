;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flatten-file - flatten out **INCLUDE** in a source.

(define readfile             ; return a list of the s-expressions in a file
  (lambda (f)
    (letrec ((rf (lambda ()
                   (let ((x (read)))
                     (if (eof-object? x)
                         ()
                         (cons x (rf)))))))
      (with-input-from-file f rf))))

(define flatten-file
  (lambda (root to)
    (let* ((u (open-output-file to))
           (flat (flatten (readfile root))))
      (for-each
        (lambda (x) (pretty-print x u))
        flat)
      (close-output-port u))))
      
(define flatten
  (lambda (s)
    (cond ((pair? s)
           (if (and (pair? (car s))
                    (equal? (caar s) '**INCLUDE**)
                    (pair? (cdar s))
                    (string? (cadar s))
                    (null? (cddar s)))
               (let* ((file (cadar s))
                      (rest (cdr s))
                      (flat (flatten (readfile file))))
                 (append flat (flatten rest)))
               (cons (flatten (car s)) (flatten (cdr s)))))
          ((eq? s '**INCLUDE**)
           (printf "Warning: may have missed an **INCLUDE**~n")
           '**INCLUDE**)
          ((vector? s)
           (list->vector (map flatten (vector->list s))))
          (else
            s))))
