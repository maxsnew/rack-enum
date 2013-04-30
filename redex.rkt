#lang racket/base
(require redex/private/match-a-pattern
	 redex/private/matcher
	 "enum.rkt"
	 racket/match)

(provide decomposition
	 pat/enum
	 sep-names
	 pattern/enum)

(struct decomposition (ctx term))

;; lang = (listof nt)
;; nt = (make-nt sym (listof rhs))
;; rhs = (make-rhs single-pattern)
;; single-pattern = sexp

;; pat/enum : pattern lang -> Enum term
(define (pat/enum pat nt-pats)
  (enum-names (sep-names pat nt-pats)
	      nt-pats
	      pat))

;; sep-names : single-pattern lang -> (assoclist symbol pattern)
;; identify all names and return them in a list where the earlier
;; names must be enum'd first

;; Precedence: mismatch repeat -> repeat names -> mismatch names ->
;; names -> subnames
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
      (loop (lookup nt-pats id) named-pats)]
     ;; 
     [`(name ,name ,pat)
      (loop pat
	    (add-if-new name pat named-pats))]
     ;; 
     [`(mismatch-name ,name ,pat)
      (loop pat
	    (add-if-new name pat named-pats))]
     [`(in-hole ,p1 ,p2)
      (loop p2
	    (loop p1 named-pats))]
     [`(hide-hole ,p) (loop p named-pats)]
     [`(side-condition ,p ,g ,e) ;; error
      (error 'no-enum)]
     [`(cross ,s)
      (error 'no-enum)] ;; error
     [`(list ,sub-pats ...)
      (foldl (λ (sub-pat named-pats)
		(match sub-pat
		  ;; unnamed repeat
		  [`(repeat ,pat #f #f)
		   (loop pat named-pats)]
		  ;; named repeat 
		  [`(repeat ,pat ,name #f)
		   (loop pat
			 (add-if-new name 'name-r named-pats))]
		  ;; mismatch named repeat
		  [`(repeat ,pat #f ,mismatch)
		   (loop pat
			 (add-if-new mismatch 'mismatch-r named-pats))]
		  ;; normal subpattern
		  [else (loop sub-pat named-pats)]))
	     named-pats
	     sub-pats)]
     [(? (compose not pair?))
      named-pats])))

(define (add-if-new k v l)
  (cond [(assoc k l) l]
	[else (cons `(,k ,v) l)]))

;; (assoclist name pattern), (hash symbol pattern), pattern -> enum term
(define (enum-names named-pats nt-pats pat)
  (let rec ([named-pats named-pats]
	    [env (hash)])
    (cond [(null? named-pats) (pattern/enum pat nt-pats env)]
	  [else
	   (match
	     (car named-pats)
	     ;; named repeat
	     [`(,name name-r)
	      (dep/enum +inf.f
			nats
			(λ (n)
			   (rec (cdr named-pats)
				(hash-set env
					  name
					  n))))]
	     ;; mismatch repeat
	     [`(,name mismatch-r)
	      (error 'unimpl)]
	     ;; named/mismatch
	     [`(,name ,pat)
	      (dep/enum +inf.f ;; suspect
			(pattern/enum pat nt-pats env)
			(λ (term)
			   (rec (cdr named-pats)
				(hash-set env
					  name
					  term))))]
	     [else (error 'bad-assoc)])])))

;; 2 passes, first identify the names
;; then make the enumerators dependent on the names
;; pattern lang (hash symbol term) -> enum term
(define (pattern/enum pat nt-pats named-terms)
  (let loop ([pat pat]
	     [n 0])
#;    (displayln pat)
    (unless (< n 100)
      (error 'inf-prob))
    (match-a-pattern
     pat
     [`any ;;
      (sum/enum
       any/enum
       (listof/enum any/enum))]
     [`number num/enum]
     [`string string/enum]
     [`natural natural/enum]
     [`integer integer/enum]
     [`real real/enum]
     [`boolean bool/enum]
     [`variable var/enum]
     [`(variable-except ,s ...)
      ;; todo
      (void)]
     [`(variable-prefix ,s)
      ;; todo
      (void)]
     [`variable-not-otherwise-mentioned
      (error 'no-enum)] ;; error
     [`hole
      (const/enum 'hole)]
     [`(nt ,id)
      (thunk/enum
       +inf.f ;; definitely wrong!
       (λ ()
	  (apply sum/enum
		 (map
		  (λ (rhs)
		     (loop (rhs-pattern rhs) (+ n 1)))
		  (lookup nt-pats id)))))]
     [`(name ,name ,pat)
      (const/enum (hash-ref named-terms name))]
     [`(mismatch-name ,name ,pat)
      (except/enum
       (loop pat (+ n 1))
       (hash-ref named-terms name))]
     [`(in-hole ,p1 ,p2)
      (map/enum
       (λ (t1-t2)
	  (decomposition (car t1-t2)
			 (cdr t1-t2)))
       (λ (decomp)
	  (cons (decomposition-ctx decomp)
		(decomposition-term decomp)))
       (prod/enum
	(loop p1 (+ n 1))
	(loop p2 (+ n 1))))]
     [`(hide-hole ,p)
      ;; todo
      (loop p (+ n 1))]
     [`(side-condition ,p ,g ,e) ;; error
      (error 'no-enum)]
     [`(cross ,s)
      (error 'no-enum)] ;; error
     [`(list ,sub-pats ...)
      ;; enum-list
      (list/enum
       (map
	(λ (sub-pat)
	   (match sub-pat
	     [`(repeat ,pat #f #f)
	      (dep/enum
	       nats
	       (λ (n)
		  (build-list n (const (loop pat (+ n 1))))))]
	     [`(repeat ,pat ,name #f)
	      (build-list (hash-ref named-terms name)
			  (const (loop pat (+ n 1))))]
	     [`(repeat ,pat #f ,mismatch)
	      (error 'unimpl)]
	     [else (loop sub-pat (+ n 1))]))
	sub-pats))]
     [(? (compose not pair?)) 
      (const/enum pat)])))

;; lookup : lang symbol -> (listof rhs)
(define (lookup nts name)
  (let rec ([nts nts])
    (cond [(null? nts) (error 'unkown-nt)]
	  [(eq? name (nt-name (car nts)))
	   (nt-rhs (car nts))]
	  [else (rec (cdr nts))])))

(define (const x)
  (λ () x))
(define natural/enum nats)

(define char/enum
  (map/enum
   integer->char
   char->integer
   (range/enum #x61 #x7a)))

(define string/enum
  (map/enum
   list->string
   string->list
   (listof/enum char/enum)))

(define integer/enum
  (sum/enum nats
	    (map/enum (λ (n) (- (+ n 1)))
		      (λ (n) (- (- n) 1))
		      nats)))

(define real/enum (from-list/enum '(0.0 1.5 123.112354)))
(define num/enum
  (sum/enum natural/enum
	    integer/enum
	    real/enum))

(define bool/enum
  (from-list/enum '(#t #f)))

(define var/enum
  (map/enum
   (compose string->symbol list->string list)
   (compose car string->list symbol->string)
   char/enum))

(define any/enum
  (sum/enum num/enum
	    string/enum
	    bool/enum
	    var/enum))

