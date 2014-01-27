#lang racket

(require redex
         redex/private/enumerator)
(define (errout e) (error 'no-encode))

(define-language STLC
  (e ::= (e e) (+ e e) x v)
  (v ::= (λ (x τ) e) n)

  (n ::= number)
  (x ::= variable-not-otherwise-mentioned)
  
  (τ ::= num (τ → τ))
  (Γ ::= (x τ Γ) •))

(define-judgment-form STLC
  #:mode (tc I I O)
  [--------------
   (tc Γ n num)]
  [(where τ (lookup Γ x))
   ----------------------
   (tc Γ x τ)]
  [(tc (x τ_x Γ) e τ_e)
   ---------------------------------
   (tc Γ (λ (x τ_x) e) (τ_x → τ_e))]
  [(tc Γ e_1 (τ_2 → τ)) (tc Γ e_2 τ_2)
   -----------------------------------
   (tc Γ (e_1 e_2) τ)]
  [(tc Γ e_0 num) (tc Γ e_1 num)
   -----------------------------
   (tc Γ (+ e_0 e_1) num)])

(define-metafunction STLC
  [(lookup (x τ Γ) x)
   τ]
  [(lookup (x_1 τ Γ) x_2)
   (lookup Γ x_2)]
  [(lookup • x)
   #f])

(define Γ/e
  (enum +inf.f
        (generate-term STLC Γ #:i-th)
        errout))
(define τ/e
  (enum +inf.f
        (generate-term STLC τ #:i-th)
        errout))
(define e/e
  (enum +inf.f
        (generate-term STLC e #:i-th)
        errout))
(define n/e
  (enum +inf.f
        (generate-term STLC n #:i-th)
        errout))

(define (tc-typ/e typ)
  (define jdg-num/e
    (fix/e +inf.f
           (λ (jdg-num/e)
              (sum/e
               (map/e
                (λ (gamma n)
                   `(tc ,gamma ,n num))
                errout
                Γ/e
                n/e)
               ))))
  (match typ
    ['num jdg-num/e]
    [_ empty/e]))

(define (tc-nv-typ/e nv typ)
  )


(for/list ([i (in-range 100)])
  (decode jdg-num/e (+ i 100)))
