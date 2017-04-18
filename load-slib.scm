;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Soft Type reconstruction for Scheme, SLIB version

;; Installation options:

;; match-file must be an absolute pathname or #f
(define match-file "/home/wright/soft/scheme/release/match-slib.scm")

;; customization-file must be an absolute pathname or #f
(define customization-file #f)

;; source-directory must be an absolute pathname
(define source-directory "/home/wright/soft/scheme/release/")

;;;;;;;;;;;;;;;;

(define soft:version "SLIB Version 0.08, Jan 11, 1994")

(and match-file (load match-file))

(define home-directory
  (or (getenv "HOME")
      (error "environment variable HOME is not defined")))

(defmacro recur args
  `(let ,@args))

(defmacro parameterize args
  (match args
    ((bindings exp ...)
     `(begin ,@exp))))

(define gensym gentemp)
(define expand-once macroexpand-1)

(**INCLUDE** "encapsulate.ss")

(**INCLUDE** "basiclib-slib.scm")
(**INCLUDE** "checklib.ss")

(encapsulate
  (st:help st: st:check st:run st:type st:ltype st:cause st:typemode
    st:summary st:verbose)

  (string-append source-directory "intro-slib.scm")

  (string-append source-directory "library.ss")
  (string-append source-directory "data.ss")

  (string-append source-directory "parse.ss")
  (string-append source-directory "macros.ss")
  (string-append source-directory "env.ss")
  (string-append source-directory "bind.ss")
  (string-append source-directory "improve.ss")
  (string-append source-directory "improve-match.ss")

  (string-append source-directory "components.ss")
  (string-append source-directory "top-sort.ss")

  (string-append source-directory "types.ss")
  (string-append source-directory "tidy.ss")
  (string-append source-directory "init.ss")
  (string-append source-directory "typechk.ss")

  (string-append source-directory "unparse.ss")
  (string-append source-directory "display-check.ss")
  (string-append source-directory "display-cause.ss")
  (string-append source-directory "display-type.ss")

  (string-append source-directory "drivers.ss"))

(and customization-file (load customization-file))
(let ((softrc (string-append home-directory "/.softschemerc")))
  (and (file-exists? softrc) (load softrc)))

(st:help)
