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

(let ([rec-nt-terms (find-recs reclang)])
  (test-begin
   (check-equal? (map cdr (hash-ref rec-nt-terms 'n)) '(#f))
   (check-equal? (map cdr (hash-ref rec-nt-terms 'v)) '(#f))
   (check-equal? (map cdr (hash-ref rec-nt-terms 'e)) '(#t #t #f))
   (check-equal? (map cdr (hash-ref rec-nt-terms 'ee)) '(#t))))

(define-language L
  (e (e_1 e_1)
     variable)
  (n number))

(define lang1 (compiled-lang-lang L))

(define eterms (pat/enum `(nt e) (compiled-lang-lang L)))
#;(define nterms (pat/enum `(list (name e_1 (nt e)) number (name e_1 (nt e))) (compiled-lang-lang L)))

(define confidence 10)
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
#; (check-equal? (size nterms) +inf.f)
 #;
 (check-enum-prop nterms
		  (λ (t)
		     (match t
		       [`((named e_1 ,e1) (,e1 ,n ,e1)) (number? n)]
		       [else #f])))
 )


(define-language fast-L
  (e variable
     (e_1 e e_1)))
(define fast-lang (compiled-lang-lang fast-L))

(define-language slow-L
  (e variable
     (e_1 variable e_1)))
(define slow-lang (compiled-lang-lang slow-L))

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
(define-language Λc
  (e (e e)
     (λ (x) e)
     x)
  (x variable))

(define λc (compiled-lang-lang Λc))
