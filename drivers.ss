;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drivers

(define init!
  (lambda ()
    (init-types!)
    (init-env!)
    (set! global-env initial-env)))

(define tree ())
(define global-env ())

(define verbose #f)

(define st:verbose
  (lambda ()
    (if verbose
        (printf "Quiet mode~%")
        (printf "Verbose mode~%"))
    (set! verbose (not verbose))))

(define cons-mutators '(set-car! set-cdr!))

(define st:check
  (lambda args
    (let ((output (apply do-soft args)))
      (when output
        (printf "Typed program written to file ~a~%" output)))))
    
(define st:run
  (lambda (file)
    (parameterize ((optimize-level 3))
      (load file))))

(define st:
  (lambda args
    (let ((output (apply do-soft args)))
      (cond
        ((not output)
         (use-error "Output file name required to run"))
        ((= 0 n-unbound)
         (printf "Typed program written to file ~a, executing ...~%" output)
         (flush-output)
         (st:run output))
        (else
         (printf "Typed program written to file ~a, not executing (unbound refs)~%" output))))))

(define do-soft
  ; returns the output file name
  (match-lambda*
    ((input (? string? output))
     (when (strip-suffix output)
       (use-error "output file name cannot end in .ss or .scm"))
     (cond ((string? input)
            (soft-files (list input))
            (check output)
            output)
           ((and (list? input) (andmap string? input))
            (soft-files input)
            (check output)
            output)
           (else
            (soft-def input)
            (check output)
            output)))
    ((input #f)
     (cond ((string? input)
            (soft-files (list input))
            #f)
           ((and (list? input) (andmap string? input))
            (soft-files input)
            #f)
           (else
            (soft-def input)
            #f)))
    ((input)
     (cond ((string? input)
            (soft-files (list input))
            (let ((o (string-append (or (strip-suffix input) input) ".soft")))
              (check o)
              o))
           ((and (list? input) (andmap string? input))
            (use-error "Output file name required"))
           (else
            (soft-def input)
            (check #f)
            #f)))
    (else
     (use-error "Input must be a file name or list of file names"))))

(define st:typemode
  (let ((mode #t))
    (lambda ()
      (if mode
          (begin
            (set! display-type ptype)
            (printf "Raw type display mode~%"))
          (begin
            (set! display-type tidy)
            (printf "Pretty type display mode~%")))
      (set! mode (not mode)))))
      
(define soft-def                 ; type a definition
  (lambda (m)
    (reinit-types!)
    (reinit-output!)
    (let ((p (parse-def m)))
      (if (null? p)
          (begin
            (reinit-env!)
            (set! global-env initial-env)
            (set! tree ())
            #f)
          (match-let* (((defs env tenv) (bind-defs p)))
            (set! global-env env)
            (set! tree (improve-defs defs))
            (type-defs tree)
            (summary "")
            #t)))))

(define soft-files               ; type a bunch of files
  (lambda (files)
    (let ((contents (map (lambda (f) `(begin ,@(readfile f))) files)))
      (soft-def `(begin ,@contents)))))

(define strip-suffix
  (lambda (name)
    (let ((n (string-length name)))
      (or (and (<= 3 n) (equal? ".ss" (substring name (- n 3) n))
               (substring name 0 (- n 3)))
          (and (<= 4 n) (equal? ".scm" (substring name (- n 4) n))
               (substring name 0 (- n 4)))))))

(define st:help
  (lambda ()
    (printf "Commands for Soft Scheme (~a)~%" soft:version)
    (printf "* (st:         file (output))    type check file and execute~%")
    (printf "* (st:type     (name))           print types of global defs~%")
    (printf "  (st:check    file (output))    type check file~%")
    (printf "  (st:run      file)             execute type checked file~%")
    (printf "  (st:ltype    (name))           print types of local defs~%")
    (printf "  (st:cause)                     print cause of CHECKs~%")
    (printf "  (st:typemode)                  toggle type display mode~%")
    (printf "  (st:summary)                   print summary of CHECKs~%")
    (printf "  (st:help)                      prints this message~%")
    ))

(define st:type type)
(define st:ltype localtype)
(define st:cause cause)
(define st:summary (lambda () (summary "")))

(init!)
