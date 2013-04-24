#lang racket/base
(require redex/private/match-a-pattern
	 "enum.rkt"
	 racket/match)

(struct decomposition (ctx term))
(define uniq (gensym 'redex-pat/enum))

;; 2 passes, first identify the names
;; then make the enumerators dependent on the names
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
      (let/ec k
	(values
	 (const/enum
	  (hash-ref env
		    name
		    (λ ()
		       (let-values ([(p/enum env) (loop pat env)])
			 (k p/enum (env))))))
	 env))
      (cond
       [(equal? x uniq)
	]
       )]
     [`(mismatch-name ,name ,pat)
      ;; 
      (loop pat env)
      ]
     [`(in-hole ,p1 ,p2)
      (let*-values ([(p1/enum env) (loop p1 env)]
		    [(p2/enum env) (loop p2 env)]))
      (values
       (map/enum
	(λ (ctx-term)
	   (decomposition (car ctx-term)
			  (cdr ctx-term)))
	;; plug-hole
	(λ (x)
	   (cons (decomposition-ctx x)
		 (decomposition-term x))) ;;unplug-hole ;; impossible?
	(prod/enum
	 p1/enum
	 p2/enum))
       env)]
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


(define plug-hole (p1-p2)
  (let ([plug (cdr p2-p2)]
	[hollow (car p1-p2)])
    ))
