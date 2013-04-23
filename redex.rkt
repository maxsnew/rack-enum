#lang racket/base
(require redex/private/match-a-pattern
	 "enum.rkt"
	 racket/match)

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
  (let ([plug (car p1-p2)]
	[hollow (cdr p2-p2)])
    ))

(define (pat/enum pat nt-pats)
  (let loop ([pat pat]
	     [env (hash)])
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
     
     [`(variable-except ,s ...) (void)]
     [`(variable-prefix ,s) (void)]
     
     [`variable-not-otherwise-mentioned (error 'no-enum)] ;; error
     [`hole
      (const/enum 'hole)]
     
     [`(nt ,id) (void)]
     [`(name ,name ,pat) (loop pat)]
     [`(mismatch-name ,name ,pat) (loop pat)]
     [`(in-hole ,p1 ,p2) 
      (map/enum
       plug-hole
       unplug-hole ;; impossible?
       (prod/enum
	(loop p1)
	(loop p2)))]
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

