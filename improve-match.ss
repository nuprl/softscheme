;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Improve a pattern match by massaging the clauses to
;; be disjoint.  When an earlier clause is more specific
;; than a later clause, we can learn that certain things cannot
;; appear.  Add not patterns to the later pattern to represent this.
;; Eg, transform the patterns:
;;        (#f body1)
;;        (x body2)
;; to:
;;        (#f body1)
;;        ((and x (not #f)) body2)

(define improve-clauses
  (lambda (clauses)
    (recur loop ((clauses clauses))
      (match clauses
        (() ())
        ((_) clauses)
        (((and m1 ($ Mclause p _ fail)) . rest)
         (let* ((rest
                 (loop rest))
                (rest
                 (if fail
                     rest
                     (recur loop2 ((clauses rest))
                       (match clauses
                         (() ())
                         (((and m ($ Mclause p2 body2 fail2)) . rest)
                          (handle
                            (cons (make-Mclause
                                    (improve-by-pattern p2 p)
                                    body2
                                    fail2)
                                  rest)
                            (match-lambda*
                              (('redundant p3)
                               (unless (null? rest)
                                 (printf "Warning: redundant pattern ~a~%"
                                   (ppat p2)))
                               (cons (make-Mclause p3 body2 fail2)
                                     rest))
                              (('continue p3)
                               (cons (make-Mclause p3 body2 fail2)
                                     (loop2 rest)))))))))))
           (cons m1 rest)))))))

; Improve p2 by p1.
; Assumes and-patterns have the only significant type first.
(define improve-by-pattern
  (lambda (p2 p1)
    (let* ((reject (lambda () (raise 'continue p2)))
           (p1covers #t)   ; p1 covers p2 
           (p2covers #t)   ; p2 covers p1
           (p3 (recur M ((p1 p1)(p2 p2))
                 ; p2 is the clause to be improved, 
                 ; p1 is the clause to improve by.
;                 (printf "(M ~a ~a)~%" (ppat p1) (ppat p2))
                 (match (cons p1 p2)

                   ; Take care of some general cases first.
                   ; These clauses must be first.

                   ((($ Pand (a . _)) . p2)
                    (M a p2))

                   ((p1 . ($ Pand (a . b)))
                    (make-flat-Pand (cons (M p1 a) b)))

                   ((($ Pvar _) . _)
                    (unless (or (Pvar? p2) (Pany? p2)) (set! p2covers #f))
                    p2)
                   ((($ Pany) . _)
                    (unless (or (Pvar? p2) (Pany? p2)) (set! p2covers #f))
                    p2)
                   ((($ Pelse) . _)
                    (unless (or (Pvar? p2) (Pany? p2)) (set! p2covers #f))
                    p2)

                   ((_ . ($ Pvar _))
                    (unless p1covers (reject))
                    (set! p1covers #f)
                    (make-flat-Pand (list p2 (make-Pnot p1))))
                   ((_ . ($ Pany))
                    (unless p1covers (reject))
                    (set! p1covers #f)
                    (make-flat-Pand (list p2 (make-Pnot p1))))
                   ((_ . ($ Pelse))
                    (unless p1covers (reject))
                    (set! p1covers #f)
                    (make-flat-Pand (list p2 (make-Pnot p1))))

                   ; Now consider remaining kinds of patterns.
                   ; Within each kind, order may matter.  Between it doesnt.

                   ((($ Pconst a _) . ($ Pconst b _))
                    (unless (equal? a b) (reject))
                    p2)

                   ((($ Pobj tag1 a) . ($ Pobj tag2 b))
                    (unless (eq? tag1 tag2) (reject))
                    (make-Pobj tag1 (map2 M a b)))
              
                   ((($ Ppred tag1) . ($ Ppred tag2))
                    (unless (eq? tag1 tag2) (reject))
                    p2)
                   ((($ Ppred tag1) . ($ Pobj tag2 _))
                    (unless (eq? tag1 tag2) (reject))
                    (set! p2covers #f)
                    p2)
                   ((($ Ppred tag1) . ($ Pconst c tag2))
                    (unless (eq? tag1 tag2) (reject))
                    (set! p2covers #f)
                    p2)

                   (_ (reject))))))
      (when p1covers
        (raise 'redundant (make-flat-Pand (list p2 (make-Pnot p1)))))
      (unless p2covers
        (raise 'continue p3))
      p3)))

(define improve-by-noisily  ; for debugging purposes only
  (lambda (p2 p1)
    (let ((r (handle (improve-by-pattern p2 p1)
               (match-lambda*
                 ((exn-ty exn-pat)
                  (printf "~a by ~a RAISES (~a ~a)~%"
                    (ppat p2) (ppat p1) exn-ty (ppat exn-pat))
                  (raise exn-ty exn-pat))))))
      (printf "~a by ~a RETURNS ~a~%" (ppat p2) (ppat p1) (ppat r))
      r)))
