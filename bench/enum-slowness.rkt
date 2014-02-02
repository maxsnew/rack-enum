#lang racket
(require redex
         profile)

(define-language LC
  (e ::= (e e)
         (λ (x) e)
         x)
  (x ::= variable-not-otherwise-mentioned))


(define-language L
  (e (e e) leaf))

(define-language lists
  (ellipses (1 ...))
  (rec      ()
            (cons 1 rec)))

(define-syntax-rule (produce lang e exp)
  (for ([i (in-range 100)])
    (generate-term lang e #:i-th (+ i (expt 10 exp)))))

(profile (produce lists rec      4))
;;(time (produce lists ellipses 5))
;;(profile (produce L e 100))




;;(time (produce lists rec ))

;;(generate-term lists rec #:i-th 0)
