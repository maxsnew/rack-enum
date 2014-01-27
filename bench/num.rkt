#lang racket

(require redex
         redex/private/enumerator)

(define-language L
  (nat ::= Z (S nat)))
(define-judgment-form L
  #:mode (sum I I O)
  [---------------
   (sum Z nat nat)]
  [(sum nat_1 nat_2 nat_3)
   -------------------------------
   (sum (S nat_1) nat_2 (S nat_3))])

(define (errout e) (error 'no-encode))

(define Lnat/e
  (enum +inf.f
        (λ (n)
           (generate-term L nat #:i-th n))
        errout))

(define jdg-nat/e
  (fix/e +inf.f
   (λ (jdg-nat/e)
      (sum/e (map/e
              (λ (n)
                 `(sum Z ,n ,n))
              errout
              Lnat/e)
             (map/e
              (λ (jdg-nat)
                 (match jdg-nat
                   [`(sum ,n1 ,n2 ,n3)
                    `(sum (S ,n1) n2 (S ,n3))]))
              errout
              jdg-nat/e)))))

(time
 (for/list ([i (in-range 1)])
   (decode jdg-nat/e (+ i 10007))))

(time
 (for/list ([i (in-range 1)])
   (generate-term L #:satisfying (sum nat_1 nat_2 nat_3) 10007)))
