#lang racket/base
(require redex/private/match-a-pattern
	 redex/private/matcher
	 "enum.rkt"
	 racket/match)

(provide decomposition
	 pat/enum
	 sep-names
	 pattern/enum
	 find-recs
	 lookup)

(struct decomposition (ctx term))

;; lang = (listof nt)
;; nt = (make-nt sym (listof rhs))
;; rhs = (make-rhs single-pattern)
;; single-pattern = sexp

;; pat/enum : pattern lang -> Enum term
(define pat/enum
  (case-lambda
    [(p ntps)
     (let ([recs (find-recs ntps)])
       (pat/enum p
		 (sort-lang ntps recs)
		 recs))]
    [(pat nt-pats recs)
     (enum-names pat
		 nt-pats
		 (sep-names pat nt-pats)
		 recs)]))



;; find-recs : lang -> (hash symbol -o> listof bool)
;; Identifies which non-terminals are recursive
(define (find-recs nt-pats)
  (define is-rec?
    (case-lambda
      [(n) (is-rec? n (hash))]
      [(nt seen)
       (or (seen? seen (nt-name nt))
	   (ormap
	    (λ (rhs)
	       (let rec ([pat (rhs-pattern rhs)])
		 (match-a-pattern
		  pat
		  [`any #f]
		  [`number #f]
		  [`string #f]
		  [`natural #f]
		  [`integer #f]
		  [`real #f]
		  [`boolean #f]
		  [`variable #f]
		  [`(variable-except ,s ...) #f]
		  [`(variable-prefix ,s) #f]
		  [`variable-not-otherwise-mentioned #f]
		  [`hole #f]
		  [`(nt ,id)
		   (is-rec? (make-nt
			     id
			     (lookup nt-pats id))
			    (add-seen seen
				      (nt-name nt)))]
		  [`(name ,name ,pat)
		   (rec pat)]
		  [`(mismatch-name ,name ,pat)
		   (rec pat)]
		  [`(in-hole ,p1 ,p2)
		   (or (rec p1)
		       (rec p2))]
		  [`(hide-hole ,p) (rec p)]
		  [`(side-condition ,p ,g ,e) ;; error
		   (error 'no-enum)]
		  [`(cross ,s)
		   (error 'no-enum)] ;; error
		  [`(list ,sub-pats ...)
		   (ormap (λ (sub-pat)
			     (match sub-pat
			       [`(repeat ,pat ,name ,mismatch)
				(rec pat)]
			       [else (rec sub-pat)]))
			  sub-pats)]
		  [(? (compose not pair?)) #f])))
	    (nt-rhs nt)))]))
  (define (calls-rec? rhs recs)
    (let rec ([pat (rhs-pattern rhs)])
      (match-a-pattern
       pat
       [`any #f]
       [`number #f]
       [`string #f]
       [`natural #f]
       [`integer #f]
       [`real #f]
       [`boolean #f]
       [`variable #f]
       [`(variable-except ,s ...) #f]
       [`(variable-prefix ,s) #f]
       [`variable-not-otherwise-mentioned #f]
       [`hole #f]
       [`(nt ,id)
	(hash-ref recs id)]
       [`(name ,name ,pat)
	(rec pat)]
       [`(mismatch-name ,name ,pat)
	(rec pat)]
       [`(in-hole ,p1 ,p2)
	(or (rec p1)
	    (rec p2))]
       [`(hide-hole ,p) (rec p)]
       [`(side-condition ,p ,g ,e) ;; error
	(error 'no-enum)]
       [`(cross ,s)
	(error 'no-enum)] ;; error
       [`(list ,sub-pats ...)
	(ormap (λ (sub-pat)
		  (match sub-pat
		    [`(repeat ,pat ,name ,mismatch)
		     (rec pat)]
		    [else (rec sub-pat)]))
	       sub-pats)]
       [(? (compose not pair?)) #f])))
  (define (seen? m s)
    (hash-ref m s #f))
  (define (add-seen m s)
    (hash-set m s #t))
  (let ([recs
	 (foldl
	  (λ (nt m)
	     (hash-set m (nt-name nt) (is-rec? nt)))
	  (hash) nt-pats)])
    (foldl
     (λ (nt m)
	(let ([rhs (nt-rhs nt)])
	  (hash-set m (nt-name nt)
		    (map (λ (rhs)
			    (calls-rec? rhs recs))
			 rhs))))
     (hash)
     nt-pats)))

;; sort-lang : lang (hash symbol -o> bool) -> lang
(define (sort-lang nt-pats recs)
  (sort nt-pats
	(λ (nt1 nt2)
	   (and (not (hash-ref recs (nt-name nt1)))
		(hash-ref recs (nt-name nt2))))))

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
     ;; names inside nts are separate
     [`(nt ,id) named-pats]
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

;; enum-names : pattern (hash symbol -o> pattern) (assoclist name pattern)
;;              (hash symbol -o> bool) -> enum term
(define (enum-names pat nt-pats named-pats recs)
  (let rec ([named-pats named-pats]
	    [env (hash)])
    (cond [(null? named-pats) (pattern/enum pat nt-pats env recs)]
	  [else
	   (match
	     (car named-pats)
	     ;; named repeat
	     [`(,name name-r)
	      (dep/enum nats
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
	      (map/enum
	       (λ (named)
		  `((named ,name ,(car named)) ,(cdr named)))
	       (λ (sepd)
		  (match
		    sepd
		    [`((named ,name ,n-term) ,term)
		     (cons n-term term)]))
	       (dep/enum (pattern/enum pat nt-pats env recs)
			 (λ (term)
			    (rec (cdr named-pats)
				 (hash-set env
					   name
					   term)))))]
	     [else (error 'bad-assoc)])])))

;; 2 passes, first identify the names
;; then make the enumerators dependent on the names
;; pattern lang (hash symbol -o> term) (hash symbol -o> bool) -> enum term
(define (pattern/enum pat nt-pats named-terms recs)
  (let loop ([pat pat]
	     [n 0])
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
      (cond [(hash-ref recs id)
	     (thunk/enum
	      +inf.f
	      (λ ()
		 (apply sum/enum
			(map
			 (λ (rhs)
			    (pat/enum (rhs-pattern rhs)
				      nt-pats
				      recs))
			 (lookup nt-pats id)))))]
	    [else
	     (apply sum/enum
		    (map
		     (λ (rhs)
			(pat/enum (rhs-pattern rhs)
				  nt-pats
				  recs))
		     (lookup nt-pats id)))])]
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

