#lang racket
(require rackunit)
(require "enum.rkt")

(struct Expr (type expr)
	#:transparent)

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
	#:transparent)

(struct Var (name)
	#:transparent
	#:property prop:custom-write
	(λ (v port write?)
	   (display (Var-name v) port)))


;; expr must be well-typed!

(define (expr/enum base-types/enum var-names/enum)
  (dep/enum
   (if (or (= 0 (size base-types/enum))
	   (= 0 (size var-names/enum)))
       0
       +inf.f)
   (type/enum base-types/enum)
   (λ (t) ;; for each type, enumerate all terms of that type
      (typed-term/enum t (hash) base-types/enum var-names/enum))))


(define (typed-term/enum type typed-vars base-types/enum var-names/enum)
  (sum/enum
   (list/enum (hash-ref type '()))
   (sum/enum
    (lambda/enum type typed-vars base-types/enum var-names/enum)
    (app/enum type typed-vars base-types/enum var-names/enum))))


(define (lambda/enum type typed-vars base-types/enum var-names/enum)
  (if (not (Arrow? type))
      empty/enum
      (thunk/enum
       (if (= 0 (size base-types/enum))
	   0
	   +inf.f)
       (λ ()
	  (dep/enum
	   (if (or (= 0 (size base-types/enum))
		   (= 0 (size var-names/enum)))
	       0
	       +inf.f)
	   var-names/enum
	   (λ (v)
	      (typed-term/enum
	       (Arrow-codom type)
	       (add-type typed-vars v (Arrow-dom type))
	       base-types/enum var-names/enum)))))))

;; update typed-vars to include
;; typed-vars is a hash table of type (type -> setof vars)
(define (add-type typed-vars var type)
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

(define (app/enum type typed-vars base-types/enum var-names/enum)
   empty/enum)

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
