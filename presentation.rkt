#lang slideshow

(require slideshow/code)
(slide (t "Enumerating Countable Sets for Property-Based Testing")
       'next
       (t "...in PLT Redex"))

;; Motivation
(slide #:title "TODO: Intro/Motivation")

;; What
(slide #:title "Enumeration"
       (t "An enumeration consists of")
       (item "A Cardinality (natural number or infinite)")
       (item "An encode function : a → Nat")
       (item "A decode function : Nat → a")
       'next
       (para "Represent as struct in Racket")
       (code (struct enum (size from to)))
       'next
       (para "Manually constructing such bijections is tricky, prefer combinators"))

(slide #:title "What combinators?"
       'alts
       (list
        (list
         (t "We want to enumerate Algebraic Data Types:")
         (code (define-language λc
                 (e ::= (e e)
                        (λ (x) e)
                        x)
                 (x ::= variable-not-otherwise-mentioned)))
         (para "Need support for alternatives (disjoint union), tuples (product), recursion (fix points)"))
        (list
         (t "Redex also supports more exotic types")
         (code (define-language exotic
                 (the-same     ::= (num_1 num_1))
                 (different    ::= (num_!_1 num_!_1))
                 (0-or-more    ::= (num ...))
                 (same-len     ::= (num ..._1 string ..._1))))
         'next
         (para "Need to support some sort of dependent enumeration..."))))

(slide #:title "Design Goals"
       (t "Combinators should be")
       'next
       (item "Efficient (log of the size of the term)")
       (item "Fair (not favor one of the argument enumerations over others)"))

(slide #:title "Sum"
       (item "Set interpretation: Disjoint union")
       (item "Given two enumerations as and bs, produce an enumeration of either as or bs")
       'next
       (item "In order to distinguish between elements when encoding, have the user provide predicates that identify the elements")
       'next
       (code (define int-or-char/e 
               (disj-sum/e (cons int/e integer?)
                           (cons char/e character?)))))

(slide #:title "Sum Example"
       (t "First consider only infinite enumerations")
       (t "TODO: show the ordering of a sum of enumerations"))

(slide #:title "Nested Sum"
       (t "TODO: show why nested sum is unfair"))

(slide #:title "Sum, redefined"
       (t "Given n enumerations, produce enumerations of any of them"))

(slide #:title "Sum of many"
       )

(slide #:title "Sums of Finite Enumerations"
       (t "Easily generalizes to arbitrary sums of finite, infinite enumerations"))

(slide #:title "Product"
       (item "Set interpretation: Cartesian Product")
       (item "Given two enumerations as and bs, produce an enumeration of pairs of as and bs")
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
       (t "TODO: fix/e")
       (t "TODO: thunk/e for mutual recursion")
       (t "TODO: caveats: order matters, we can't figure out size for you"))

(slide #:title "Dependence"
       (t "Set interpretation: countable union")
       (t "Type theory interpretation: Dependent product")
       (t "TODO: ")
       (t "Slow on some inputs!"))


;; How
(slide #:title "Applications"
       (item "Testing")
       (item "Games"))

(slide #:title "PLT Redex"
       (item "DSL for Formal Semantics")
       (item "Debugging formal languages"))

(slide #:title "Algebraic Data Types"
       (item "Map directly to combinators")
       'alts
       (list
        (list 
         (code (define-language λc
               (e ::= (e e)
                      (λ (x) e)
                      x)
               (x ::= variable-not-otherwise-mentioned))))
        (list 
         (code (define e/e (disj-sum/e (x/e . x?)
                                       ((list/e (fin/e λ)
                                                (list/e x/e)
                                                e/e)
                                        . lam?)
                                       ((list/e e/e e/e)
                                        . app?)))
               (define x/e var/e)))))
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

(slide #:title "Fairness...")
;; Who
#;
(slide #:title "Thanks"
       (item "Robby Findler")
       (item "Paul Tarau")
       (item "Jay McCarthy"))