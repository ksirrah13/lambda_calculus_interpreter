#lang sicp

(define (eval exp env)
  (cond ((symbol? exp) (lookup-value exp env))
        ((lambda? exp) (make-closure exp env))
        (else (do-application exp env))))

(define (apply closure args)
  (if (closure? closure)
      (eval (get-body closure)
            (extend-env (get-var closure) args (get-env closure)))
      (list closure args)))
  
(define (closure? exp)
  (and (pair? exp)
       (pair? (car exp))
       (lambda? (car exp))))

(define (lambda? exp)
      (eq? (car exp) '/))

(define (lookup-value var env)
  (let ((result (assq var env)))
    (if result
        (cadr result)
        var)))

(define (make-closure l env)
  (cons (simplify l env) env))

(define (do-application exp env)
  (apply (eval (car exp) env)
         (eval (cadr exp) env)))

(define (get-body closure)
  (cddar closure))

(define (get-var closure)
  (cadar closure))

(define (get-env closure)
  (cdr closure))

(define (extend-env var arg env)
  (cons (list var arg) env))

(define (simplify l env)
  (cond ((symbol? l) (eval l env))
        ((lambda? l) (simplify-lambda l env))
        ((symbol? (eval (car l) env)) l)
        (else (eval l env))))

(define (simplify-lambda l env)
  (let ((var (cadr l)))
    (cons '/ (cons var (simplify (cddr l) env)))))

(define (do-eval exp)
  (let ((result (eval exp '())))
    (if (closure? result)
        (car result)
        result)))

(do-eval '(/ t . (/ f . t)))
; {/ t / f . t}

(do-eval '((/ x . x) (/ a . a)))
; {/ a . a}

(do-eval '((/ f . (f (/ a . a))) (/ x . x)))
; {/ a . a}

(do-eval '((/ b . (/ t . (/ f . ((b f) t)))) (/ t . (/ f . t))))
; {/ t / f . t} - naming problem

(do-eval '((/ b . (/ t . (/ f . ((b f) t)))) (/ x . (/ y . x))))
; {/ t / f . f}
