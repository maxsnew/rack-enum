#lang racket/base
(require redex/private/match-a-pattern
	 "enum.rkt"
	 racket/match)


(define (pat/enum pat nt-pats)
  (let loop ([pat pat]
	     [env (hash)])
    (match-a-pattern
     pat
     [`any ;;
      (sum/enum
       (loop `number env)
       (loop `string env)
       (loop `boolean env)
       (loop `variable env)
       (thunk/enum
	+inf.f
	(λ ()
	   (prod/enum
	    (loop `any env)
	    (loop `any env)))))]
     [`number num/enum]
     [`string string/enum]
     [`natural natural/enum]
     [`integer integer/enum]
     [`real real/enum]
     [`boolean bool/enum]
     [`variable var/enum]
     [`(variable-except ,s ...) (void)]
     [`(variable-prefix ,s) (void)]
     [`variable-not-otherwise-mentioned (void)] ;; error
     [`hole (void)]
     [`(nt ,id) (void)]
     [`(name ,name ,pat) (loop pat)]
     [`(mismatch-name ,name ,pat) (loop pat)]
     [`(in-hole ,p1 ,p2) 
      (loop p1)
      (loop p2)]
     [`(hide-hole ,p) (loop p)]
     [`(side-condition ,p ,g ,e) ;; error
      (loop p)]
     [`(cross ,s) (void)] ;; error
     [`(list ,sub-pats ...)
      (for ([sub-pat (in-list sub-pats)])
	(match sub-pat
	  [`(repeat ,pat ,name ,mismatch)
	   (loop pat)]
	  [else
	   (loop sub-pat)]))]
     [(? (compose not pair?)) 
      (const/enum pat)])))


