#lang slideshow

(require racket/draw 
         redex/private/enumerator
         slideshow/code
         "../enum-util.rkt"
         "../results/plot.rkt"
         "../util.rkt")
(slide (t "Enumerating Countable Sets for Property-Based Testing"))

;; Motivation
(slide (t "First, a demo"))

;; What
(slide #:title "Enumeration"
       (t "An enumeration consists of")
       (item "A Cardinality (natural number or infinite)")
       (item "An encoding function to-nat  : a → Nat")
       (item "A decoding function from-nat : Nat → a"))

(slide #:title "Examples"
       (item "Natural numbers: infinite, identity, identity")
       (item "Booleans: 2, 0 ↔ true and 1 ↔ false")
       (item "Integers: infinite, ...")
       'next
       (para "Manually constructing such bijections is tricky, prefer combinators"))

(slide #:title "What combinators?"
       'alts
       (list
        (list
         (code (define-language rbtrees
                 (tree ::= empty
                           (node color val tree tree))))
                 
         (para "Need support for alternatives, tuples, recursion (fix points)"))
        (list
         (t "Redex also supports more exotic types")
         ;; TODO: change this to be a table of pattern <-> combinator feature
         (code (define-language exotic
                 (the-same     ::= (num_1 num_1))
                 (different    ::= (num_!_1 num_!_1))
                 (0-or-more    ::= (num ...))
                 (same-len     ::= (num ..._1 string ..._1))))
         'next
         (para "Need to support some sort of dependent enumeration..."))))

(slide #:title "Design Goals"
       (t "Combinators should be")
       (item "Efficient (produced enumerations should have linear complexity in the length of the bitstring of the input number)")
       (item "Fair (not favor one of the argument enumerations over others)"))

;; TODO: better version of this...
(define (enum-col e n)
  
  (define to-str number->string)
  (foldr vl-append 
         (blank)
         (for/list ([x (in-list (approximate e n))])
           (text (to-str x)))))
(slide #:title "Sum"
       (item "Set interpretation: Disjoint union")
       (item "disj-sum/e : enum a, enum b → enum (a or b)")
       (code (define int-or-char/e 
               (disj-sum/e int/e char/e)))
       'next
       )
(define neg/e 
  (map/e (λ (x) (sub1 (- x)))
         (λ (x) (- (add1 x)))
         nat/e))
(slide #:title "Sum Example"
       (t "First consider only infinite enumerations")
       'alts
       (list 
        (list
         (htl-append (enum-col nat/e 10)
                   (enum-col neg/e 10)))
        (list (enum-col (disj-sum/e (cons nat/e number?) (cons neg/e number?)) 20))))

(slide #:title "from-nat"
       (t "Just check if it's even or odd (constant time)"))

(slide #:title "Nested Sum"
       
       (t "TODO: show why nested sum is unfair"))

(slide #:title "Sum, redefined"
       (item "disj-sum/e : enum a_1, enum a_2, ... → enum (a_1 or a_2 or ...)"))

(slide #:title "Sum of many"
       (t "TODO: ")
       )

(slide #:title "Sums of Finite Enumerations"
       (t "Easily generalizes to arbitrary sums of finite, infinite enumerations"))

(slide #:title "Product"
       (item "Set interpretation: Cartesian Product")
       (item "cons/e : enum a, enum b → enum (a, b)")
       'next
       (code (define int-and-char/e (cons/e int/e char/e))))

(slide #:title "Product Example"
       (t "TODO: show picture of table")
       'next
       (t "What order do we want?"))

(slide #:title "Cantor Pairing Function"
       (t "Familiar decode function")
       (t "TODO: picture of Cantor pairing enumeration")
       (t "TODO: derivation of encode"))

(slide #:title "Deriving encode"
       (para "Given an injective and surjective encode function, we can always"
             "construct a decode function by brute-force search.")
       (para "Obviously this is too inefficient for practical use")
       (item "Need to solve a quadratic Diophantine equation")
       (item "Fortunately, an efficient solution is known: TODO"))

(slide #:title "Nested Pairing"
       (t "Once again nesting is too unfair to be used in general"))

(slide #:title "Generalized Cantor N-Tupling"
       (t "Known \"fair\" generalization to Skolem at latest")
       (para "But, encode?")
       'next
       (item "n-th degree Diophantine equation...")
       (item "Known search procedure (Knuth) but still too inefficient"))

(slide #:title "Back to the drawing board..."
       (t "Cantor tupling orders by the sum of the indices, what about the max?")
       (para "Instead of searching by layers of an n-simplex (triangle, tetrahedron)"
             "search by layers of an n-cube."))

(slide #:title "Boxy N-Tupling"
       (t "TODO: picture of boxy enumeration")
       (para "encode: just need n-th root!"))

(slide #:title "Mixed finite/infinite N-tupling")

(slide #:title "Fair?"
       (t "More on this later..."))

(slide #:title "Recursion"
       (code (fix/e (λ (l/e) (disj-sum/e (fin/e '())
                                         (cons/e nat/e l/e)))))
       (t "TODO: fix/e")
       (t "TODO: thunk/e for mutual recursion")
       (t "TODO: caveats: order matters, we can't figure out size for you"))

(slide #:title "Dependence"
       (t "Set interpretation: union of an indexed family of sets")
       (t "TODO: ")
       (t "Slow on some inputs!"))

(slide #:title "Filter"
       (t "Set interpretation: subset")
       (t "General filter is slow/hard")
       (t "But removing finitely many (known) elements is easy!")
       (t "except/e : enum a, a → enum a"))

;; How

(slide #:title "Applications"
       (item "Testing")
       (item "Games"))

(slide #:title "Exotic patterns"
       'alts
       (list
        (list (item "Extract all variables into an environment, then plug them in at the end")
              'alts
              (list
               (list (code (same ::= (num_1 num_1))))
               (list (code (same/e = (map/e plug 
                                            unplug
                                            (cons/e )))))
               )
              )))

(slide #:title "Evaluation"
       (item "What's the best way to use enumerations for testing?")
       (item "How does the enumeration compare to (ad-hoc) random generators?"))

(slide #:title "Enumeration Generation"
       'alts
       (list
        (list
         (item "In-order enumeration")
         (item "Known technique: see SmallCheck")
         (item "Deterministic"))
        (list
         (item "Random natural number indexing into an enumeration")
         (item "How to select a natural number?")
         (item "Sample from a geometric distribution, then pick an index between 2^n, 2^(n+1)")
         (item "Sensitive to the probability of 0, branching factor of the grammar"))))

(slide #:title "Comparison"
       (item "3 techniques: Old Random Generator, Random natural indexing, In-order enumeration")
       (item "6 Redex models with 3-9 bugs each"))

(slide #:title "Raw Results"
       (bitmap (make-object bitmap% "../pict_3.png")))



(slide #:title "Fairness...")
;; Who
(slide #:title "Related Work"
       (item "Enumeration")
       (subitem "Paul Tarau. Bijective Term Encodings.") (comment "Doesn't handle dependency or finite terms")
       (subitem "Duregård et al. FEAT: Functional Enumeration of Algebraic Types") (comment "Doesn't handle dependency")
       (item "Automated Testing")
       (subitem "Runciman et al. SmallCheck and Lazy SmallCheck")
       (subitem ""))
(slide #:title "Thanks"
       (item "Robby Findler")
       (item "Paul Tarau")
       (item "Jay McCarthy"))