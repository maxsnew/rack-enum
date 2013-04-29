#lang racket/base
(require rackunit
	 racket/list)
(require "enum.rkt")

(struct Lambda (var expr)
	#:prefab)
(struct App (rator rand)
	#:prefab)
(struct Var (v)
	#:prefab)

(define (expr-to-list e)
  (cond [(Var? e) (Var-v e)]
	[(App? e) (list (expr-to-list (App-rator e))
			(expr-to-list (App-rand e)))]
	[(Lambda? e)
	 (list 'λ
	       (list (Var-v (Lambda-var e)))
	       (expr-to-list (Lambda-expr e)))]))

(define (l-expr/enum vars)
  (map/enum
   expr-to-list
   (λ (x) x)
   (expr/enum empty/enum (from-list/enum (map Var vars)))))

(define (expr/enum in-scope/enum vars/enum)
  (thunk/enum
   (if (= 0 (size vars/enum))
       0
       +inf.f)
   (λ ()
      (sum/enum
       in-scope/enum
       (sum/enum
	(lambda/enum in-scope/enum vars/enum)
	(app/enum in-scope/enum vars/enum))))))

(define (lambda/enum in-scope/enum vars/enum)
  (thunk/enum
   (if (= 0 (size vars/enum))
       0
       +inf.f)
   (λ ()
      (map/enum
       (λ (ve)
	  (Lambda (car ve) (cdr ve)))
       (λ (l)
	  (cons (Lambda-var l)
		(Lambda-expr l)))
       (dep/enum
	+inf.f
	vars/enum
	(λ (v)
	   (expr/enum
	    (sum/enum (const/enum v)
		      in-scope/enum)
	    vars/enum)))))))

(define (app/enum in-scope/enum vars/enum)
  (map/enum
   (λ (rand-rator)
      (App (car rand-rator) (cdr rand-rator)))
   (λ (app)
      (cons (App-rand app)
	    (App-rator app)))
   (thunk/enum
    (if (= 0 (size vars/enum))
	0
	+inf.f)
    (λ ()
       (prod/enum (expr/enum in-scope/enum vars/enum)
		  (expr/enum in-scope/enum vars/enum))))))

(define zero (lambda/enum empty/enum empty/enum))
(define ls (lambda/enum empty/enum (const/enum (Var 'x))))
(define x-exprs (l-expr/enum '(x)))
(define exprs (l-expr/enum '(x)))

(define (show-exprs n)
  (for ([i (range n)])
    (display (decode exprs i))
    (newline)
    (newline)))

(define (make-exprs n)
  (for ([i (range n)])
    (decode exprs i)))
