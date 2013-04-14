#lang racket
(require rackunit)
(require "enum.rkt")

(struct Expr (type expr)
	#:transparent
	#:property prop:custom-write
	(λ (e port write?)
	   (display (Expr-expr e) port)))

(struct Lambda (var type expr)
	#:transparent
	#:property prop:custom-write
	(λ (l port write?)
	   (display
	    (list 'λ (list (Lambda-var l) ': (Lambda-type l))
		  (Lambda-expr l))
	    port)))

(struct Type (name)
	#:transparent
	#:property prop:custom-write
	(λ (t port write?)
	   (display (Type-name t) port)))

(struct Arrow (dom codom)
	#:transparent
	#:property prop:custom-write
	(λ (arr port write?)
	   (display (list (Arrow-dom arr)
			  '->
			  (Arrow-codom arr))  port)))

(struct App (rator rand)
	#:transparent
	#:property prop:custom-write
	(λ (app port write?)
	   (display (list (App-rator app) (App-rand app)) port)))

(struct Var (name)
	#:transparent
	#:property prop:custom-write
	(λ (v port write?)
	   (display (Var-name v) port)))


;; expr must be well-typed!
;; primitives is a map from types to values
(define (expr/enum primitives base-types/enum var-names/enum)
  (map/enum
   (λ (t-e)
      (Expr (car t-e)
	    (cdr t-e)))
   (λ (expr)
      (cons (Expr-type expr)
	    (Expr-expr expr)))
   (dep/enum
    (if (or (false? (hash-iterate-first primitives))
	    (= 0 (size base-types/enum))
	    (= 0 (size var-names/enum)))
	0
	+inf.f)
    (type/enum base-types/enum)
    (λ (t) ;; for each type, enumerate all terms of that type
       (typed-term/enum t  (hash) primitives base-types/enum var-names/enum)))))


(define (typed-term/enum type typed-vars primitives base-types/enum var-names/enum)
  ;;(display typed-vars)
  ;;(display primitives)
  (sum/enum
   (sum/enum
    (list/enum (set->list (hash-ref typed-vars type (set))))
    (list/enum (set->list (hash-ref primitives type (set)))))
   (sum/enum
    (lambda/enum type typed-vars primitives base-types/enum var-names/enum)
    (app/enum type typed-vars primitives base-types/enum var-names/enum))))


(define (lambda/enum type typed-vars primitives base-types/enum var-names/enum)
  (if (not (Arrow? type))
      empty/enum
      (map/enum
       (λ (var-expr)
	  (Lambda (car var-expr)
		  (Arrow-dom type)
		  (cdr var-expr)))
       (λ (l)
	  (cons (Lambda-var l)
		(Lambda-expr l)))
       (thunk/enum
	(if (or (= 0 (size base-types/enum))
		(= 0 (size var-names/enum)))
	    0
	    +inf.f)
	(λ ()
	   (dep/enum
	    +inf.f
	    var-names/enum
	    (λ (v)
	       (thunk/enum
		+inf.f
		(λ ()
		   (typed-term/enum
		    (Arrow-codom type)
		    (add-type typed-vars v (Arrow-dom type))
		    primitives
		    base-types/enum var-names/enum))))))))))

;; update typed-vars to include
;; typed-vars is a hash table of type (type -> setof vars)
(define (add-type typed-vars var type)
  (unless (or (Type? type)
	      (Arrow? type))
    (error 'not-a-type))
  (hash-update
   (let loop ([pos (hash-iterate-first typed-vars)])
     (cond [(false? pos) typed-vars]
	   [(set-member? (hash-iterate-value typed-vars pos) var)
	    (hash-update typed-vars
			 (hash-iterate-key typed-vars pos)
			 (λ (s)
			    (set-remove s var)))]
	   [else (loop (hash-iterate-next typed-vars pos))]))
   type
   (λ (s)
      (set-add s var))
   (set)))

(define (app/enum type typed-vars primitives base-types/enum var-names/enum)
  (map/enum
   (λ (t-rator-rand)
      (App (cadr t-rator-rand)
	   (cddr t-rator-rand)))
   (λ (app)
      (cons type
	    (cons (App-rator app)
		  (App-rand app))))
   (thunk/enum
    (if (or (= 0 (size base-types/enum))
	    (= 0 (size var-names/enum)))
	0
	+inf.f)
    (λ ()
       (dep/enum
	+inf.f
	(type/enum base-types/enum)
	(λ (t)
	   (prod/enum
	    (thunk/enum
	     +inf.f
	     (λ ()
		(typed-term/enum (Arrow t type)
				 typed-vars
				 primitives
				 base-types/enum
				 var-names/enum)))
	    (thunk/enum
	     +inf.f
	     (λ ()
		(typed-term/enum t typed-vars primitives base-types/enum var-names/enum))))))))))

(define (type/enum base-types/enum)
  (thunk/enum
   (if (= 0 (size base-types/enum))
       0
       +inf.f)
   (λ ()
      (sum/enum
       base-types/enum
       (arrow/enum base-types/enum)))))

(define (arrow/enum base-types/enum)
  (thunk/enum
   (if (= 0 (size base-types/enum))
       0
       +inf.f)
   (λ ()
      (map/enum
       (λ (lr)
	  (Arrow (car lr) (cdr lr)))
       (λ (arr)
	  (cons (Arrow-dom arr) (Arrow-codom arr)))
       (prod/enum
	(sum/enum
	 base-types/enum
	 (arrow/enum base-types/enum))
	(sum/enum
	 base-types/enum
	 (arrow/enum base-types/enum)))))))

(define es
  (expr/enum (hash (Type 'int) (set 0))
	     (const/enum (Type 'int))
	     (const/enum (Var 'x))))

(define ints
  (typed-term/enum (Type 'int)
		  (hash)
		  (hash (Type 'int) (set 0))
		  (const/enum (Type 'int))
		  (const/enum (Var 'x))))

(define with-y
  (typed-term/enum (Type 'int)
		   (hash (Type 'int) (set (Var 'y)))
		   (hash (Type 'int) (set 0))
		   (const/enum (Type 'int))
		   (const/enum (Var 'x)))) 

(define yfns
  (typed-term/enum (Arrow (Type 'int) (Type 'int))
		   (hash (Type 'int) (set (Var 'y)))
		   (hash (Type 'int) (set 0))
		   (const/enum (Type 'int))
		   (const/enum (Var 'x))))

(define stΛc
  (expr/enum (hash (Type 'bool) (set #t #f)
		   (Type 'int) (set 0)
		   (Arrow (Type 'int) (Type 'int)) (set 'add1)
		   (Arrow (Type 'bool) (Arrow (Type 'bool) (Type 'bool))) (set 'or 'and))
	     (list/enum (list (Type 'int) (Type 'bool)))
	     (list/enum (list (Var 'x) (Var 'y)))))
