#lang racket/base

(require redex/private/matcher
	 redex/private/reduction-semantics
	 "redex.rkt"
	 "enum.rkt"
	 rackunit
	 racket/list
	 racket/match)

;; lang = (listof nt)
;; nt = (make-nt sym (listof rhs))
;; rhs = (make-rhs single-pattern)
;; single-pattern = sexp


;; find-recs test
(define-language recL
  (n v)
  (e (e e)
     (λ v e)
     number)
  (v variable)
  (ee (e_1 e_1)))

(define reclang (compiled-lang-lang recL))

(let ([recs (find-recs reclang)])
  (test-begin
   (check-false (hash-ref recs 'n))
   (check-false (hash-ref recs 'v))
   (check-true (hash-ref recs 'e))
   (check-true (hash-ref recs 'ee))))

(define-language L
  (e (e_1 e_1)
     variable)
  (n number))

(define lang1 (compiled-lang-lang L))

(test-begin
 )

(define eterms (pat/enum `(nt e) (compiled-lang-lang L)))
#;(define nterms (pat/enum `(list (name e_1 (nt e)) number (name e_1 (nt e))) (compiled-lang-lang L)))

(define confidence 10000)
(define-simple-check (check-enum-prop e p)
  (for ([i (range confidence)])
    (check-true (p (decode e i)))))

(test-begin
 (check-equal? (size eterms) +inf.f)
#; (check-bijection? eterms)
 #;
 (check-enum-prop eterms
		  (λ (t)
		     (match t
		       [(? symbol?) #t]
		       [`(λ ,v ,e) (symbol? v)]
		       [`(,e1 ,e2) #t]
		       [else #f])))
 #;#; (check-equal? (size nterms) +inf.f)
 (check-enum-prop nterms
 (λ (t)
 (match t
 [`((named e_1 ,e1) (,e1 ,n ,e1)) (number? n)]
 [else #f])))
 )


(define-language named-L
  (e variable
     (x e_1 e_1)
     ))
(define named-lang (compiled-lang-lang named-L))
#;
(define clash-names (pat/enum `(list (name e_1 (nt e))
				     (name e_1 (nt e))
				     (name e_1 (nt e)))
			      named-lang))
#;
(test-begin
 (check-equal? (size clash-names) +inf.f)
 (check-enum-prop clash-names
		  (λ (t)
		     (match t
		       [`((named e_1 ,e1) (,e1 ,e1 ,e1))
			(match e1
			  [(? symbol?) #t]
			  [`((named e_1 ,e2) (,e2 ,_ ,e2))
			   #t]
			  [else #f])]
		       [else #f]))))
