#lang racket
(require rackunit)
(require "enum.rkt")

(struct Lambda (var type expr)
	#:transparent)

(struct Type (name)
	#:transparent
	#:property prop:custom-write
	(λ (t port write?)
	   (display (Type-name t) port)))
(struct Arrow (dom codom)
	#:transparent
	(λ (arr)
	   (display   port)))

(struct App (rator rand)
	#:transparent)

(struct Var (v)
	#:transparent

	)

;; expr must be well-typed!
;; (define (expr/enum ...)
;;   ...)

;; (define (lambda/enum ...)
;;   ...)

;; (define (app/enum ...)
;;   ...)

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
