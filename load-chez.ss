;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Soft Type reconstruction for Scheme, Chez version

;; Installation options:
(eval-when (compile load eval)

;; match-file must be an absolute pathname or #f
(define match-file "/home/wright/soft/scheme/release/match.so")

;; customization-file must be an absolute pathname or #f
(define customization-file "/home/wright/soft/custom-chez.ss")
)

;;;;;;;;;;;;;;;;

(define soft:version "SLIB Version 0.08, Jan 11, 1994")

(eval-when (compile load eval)
  (case-sensitive #t) ; recommended but not essential
  (print-graph #f)
  (print-gensym #f)
  (and match-file (load match-file)))

(eval-when (compile)
  (optimize-level 3))

(define home-directory "~")

(**INCLUDE** "encapsulate.ss")

(**INCLUDE** "basiclib-chez.ss")
(**INCLUDE** "checklib.ss")

(encapsulate
  (st:help st: st:check st:run st:type st:ltype st:cause st:typemode
    st:summary st:verbose)

  "intro-chez.ss"

  "library.ss"
  "data.ss"

  "parse.ss"
  "macros.ss"
  "env.ss"
  "bind.ss"
  "improve.ss"
  "improve-match.ss"

  "components.ss"
  "top-sort.ss"

  "types.ss"
  "tidy.ss"
  "init.ss"
  "typechk.ss"

  "unparse.ss"
  "display-check.ss"
  "display-cause.ss"
  "display-type.ss"

  "drivers.ss")

(and customization-file (load customization-file))
(let ((softrc (string-append home-directory "/.softschemerc")))
  (and (file-exists? softrc) (load softrc)))

(st:help)
