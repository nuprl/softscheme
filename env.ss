;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environments

(define empty-env ())

(define lookup-or-fail
  (lambda (env x fail-thunk)
    (match (assq x env)
      (#f (fail-thunk))
      ((_ . b) b))))

(define lookup
  (lambda (env x)
    (match (assq x env)
      (#f (disaster 'lookup "no binding for ~a" x))
      ((_ . b) b))))

(define bound?
  (lambda (env x)
    (match (assq x env)
      (#f #f)
      (_ #t))))

(define extend-env
  (lambda (env x v)
    (cons (cons x v) env)))

(define extend-env*
  (lambda (env xs vs)
    (append (map2 cons xs vs) env)))

(define join-env
  (lambda (env newenv)
    (append newenv env)))
