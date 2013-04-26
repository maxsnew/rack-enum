#lang racket/base
(require redex/private/match-a-pattern
	 "enum.rkt"
	 racket/match)

(struct decomposition (ctx term))
(define uniq (gensym 'redex-pat/enum))


;; sep-names : pattern (hash symbol pattern) -> listof symbol
;; identify all names and return them in a list where the earlier
;; names must be enum'd first
(define (sep-names pat nt-pats)
  (let loop ([pat pat]
	     [named-pats '()])
    (match-a-pattern
     pat
     [`any named-pats]
     [`number named-pats]
     [`string named-pats]
     [`natural named-pats]
     [`integer named-pats]
     [`real named-pats]
     [`boolean named-pats]
     [`variable named-pats]
     [`(variable-except ,s ...) named-pats]
     [`(variable-prefix ,s) named-pats]
     [`variable-not-otherwise-mentioned named-pats]
     [`hole named-pats]
     ;; should be whatever names are in that nt
     [`(nt ,id)
      (loop (hash-ref nt-pats id) named-pats)]
     [`(name ,name ,pat)
      (cond [(assoc name named-pats)
	     (loop pat named-pats)]
	    [else
	     (loop pat
		   (cons (cons name pat)
			 named-pats))])]
     [`(mismatch-name ,name ,pat)
      (cond [(assoc name named-pats)
	     (loop pat named-pats)]
	    [else
	     (loop pat
		   (cons (cons name pat)
			 named-pats))])]
     [`(in-hole ,p1 ,p2)
      (loop p2
	    (loop p1 named-pats))]
     [`(hide-hole ,p) (loop p named-pats)]
     [`(side-condition ,p ,g ,e) ;; error
      (error 'no-enum)]
     [`(cross ,s)
      (error 'no-enum)] ;; error
     [`(list ,sub-pats ...)
      ;; fold!
      (foldl (λ (sub-pat named-pats)
		(loop
		 (match sub-pat
		   [`(repeat ,pat ,name ,mismatch)
		    pat]
		   [else sub-pat])
		 named-pats))
	     named-pats
	     sub-pats)]
     [(? (compose not pair?))
      named-pats])))

;; 
(define (enum-names named-pats pat)
  (if (null? named-pats)
      (pat/enum pat)
      (dep/enum +inf.f ;; change later!!!
		(prod/enum
		 (const/enum (caar named-pats))
		 (pat/enum (cdar named-pats)))
		(λ (_)
		   (enum-names (cdr named-pats) pat)))))

;; 2 passes, first identify the names
;; then make the enumerators dependent on the names
#;
(define (pat/enum pat nt-pats)
  (let loop ([pat pat]
	     [env (hash)] ;; name -o> any
	     )
    (match-a-pattern
     pat
     [`any ;;
      (values
       (sum/enum
	any/enum
	(listof/enum any/enum))
       env)]
     [`number num/enum]
     [`string string/enum]
     [`natural natural/enum]
     [`integer integer/enum]
     [`real real/enum]
     [`boolean bool/enum]
     [`variable var/enum]
     
     [`(variable-except ,s ...) (void)]
     [`(variable-prefix ,s) (void)]
     
     [`variable-not-otherwise-mentioned (error 'no-enum)] ;; error
     [`hole
      (const/enum 'hole)]
     
     [`(nt ,id) (void)]
     [`(name ,name ,pat)
      ;; not this
      (void)]
     [`(mismatch-name ,name ,pat)
      ;; 
      (loop pat env)
      ]
     [`(in-hole ,p1 ,p2)
      (void)]
     [`(hide-hole ,p) (loop p)]
     [`(side-condition ,p ,g ,e) ;; error
      (error 'no-enum)]
     [`(cross ,s)
      (error 'no-enum)] ;; error
     [`(list ,sub-pats ...)
      (for ([sub-pat (in-list sub-pats)])
	(match sub-pat
	  [`(repeat ,pat ,name ,mismatch)
	   (loop pat)]
	  [else
	   (loop sub-pat)]))]
     [(? (compose not pair?)) 
      (const/enum pat)])))

(define natural/enum nats)

(define string/enum
  (list/enum '("" "hello" "world")))


(define integer/enum
  (sum/enum nats
	    (map/enum (λ (n) (- (+ n 1)))
		      (λ (n) (- (- n) 1))
		      nats)))

(define real/enum (list/enum '(0.0 1.5 123.112354)))
(define num/enum
  (sum/enum natural/enum
	    integer/enum
	    real/enum))


(define bool/enum
  (list/enum '(#t #f)))

(define var/enum (list/enum '(x y z)))

(define any/enum
  (sum/enum num/enum
	    string/enum
	    bool/enum
	    var/enum))

