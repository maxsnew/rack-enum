#lang racket
(require rackunit)
(require racket/pretty)
(require "enum.rkt")
(require profile)
(require math/number-theory)

(struct Leaf (val)
	#:prefab)
(struct Tree (val left right)
	#:prefab)
(struct LTree (val right)
	#:prefab)
(struct RTree (val left)
	#:prefab)

(define (val t)
  (cond [(Leaf? t)
	 (Leaf-val t)]
	[(Tree? t)
	 (Tree-val t)]))

;; Enum a -> Enum (listof a)
(define (listof/enum e)
  (thunk/enum +inf.f
	      (λ ()
		 (sum/enum
		  (const/enum '())
		  (prod/enum e (listof/enum e))))))

(define bools (list/enum '(#t #f)))
(define lbs (listof/enum bools))

(define (ranges low high)
  (cond
    [(= low high) (const/enum low)]
    [(< low high)
     (map 
      (λ (n) (range/enum ))
      
      (range/enum low high))]))

(define (bst/enum low high)
  (cond
   [(<= (- high low) 1) empty/enum]
   [(= (- high low) 2)
    (const/enum (Leaf (- high 1)))]
   ;; first choose a value
   [else
    (thunk/enum
     (if (infinite? (- high low))
	 +inf.f
	 (catalan (- high low 1)))
     (λ ()
	(map/enum
	 (λ (val-l-r)
	    (Tree (car val-l-r)
		  (cadr val-l-r)
		  (cddr val-l-r)))
	 (λ (t)
	    (cons (Tree-val t)
		  (cons (Tree-left t)
			(Tree-right t))))
	 (dep/enum
	  (- high low 1)
	  (range/enum (+ low 1) (- high 1))
	  (λ (n)
	     (prod/enum
	      (if (= (- n low) 1)
		  (const/enum '())
		  (bst/enum low n))
	      (if (= (- high n) 1)
		  (const/enum '())
		  (bst/enum n high))))))))]))

(define (catalan n)
  (quotient
   (binomial (* 2 n) n)
   (+ 1 n)))

(define-simple-check (check-tree-eq? t1 t2)
  (cond [(and (Leaf? t1) (Leaf? t2))
	 (check-equal? (Leaf-val t2)
		       (Leaf-val t2))]
	[(and (Tree? t1) (Tree? t2))
	 (begin
	   (check-equal? (Tree-val t1)
			 (Tree-val t2))
	   (check-tree-eq? (Tree-left t1)
			   (Tree-left t2))
	   (check-tree-eq? (Tree-right t1)
			   (Tree-right t2)))]))

(define zero (bst/enum 0 0))
(define zero2 (bst/enum 0 1))
(define one (bst/enum 0 2))
(define two (bst/enum 0 3))
(define five (bst/enum 0 4))
(define 0-trees
  (map/enum cdr 
            (λ (x) (cons 1 x))
            (dep/enum
	     (catalan 1)
             (nats+/enum 2)
             (λ (n)
               (bst/enum 0 n)))))
(test-begin
 (check-equal? (size zero) 0)
 (check-equal? (size zero2) 0)
 (check-equal? (size one) 1)
 (check-tree-eq? (decode one 0) (Leaf 1))
 (check-equal? (size two) 2)
 (check-tree-eq? (decode two 0) (Tree 1 (Leaf 2) '()))
 (check-tree-eq? (decode two 1) (Tree 2 '() (Leaf 1)))
 (check-equal? (size five) 5)
 (check-tree-eq? (decode five 0) (Tree 1 '() (Tree 2 '() (Leaf 3))))
 (check-tree-eq? (decode five 1) (Tree 1 '() (Tree 3 (Leaf 2) '())))
 (check-tree-eq? (decode five 2) (Tree 2 (Leaf 1) (Leaf 3)))
 (check-tree-eq? (decode five 3) (Tree 3 (Tree 1 '() (Leaf 2)) '()))
 (check-tree-eq? (decode five 4) (Tree 3 (Tree 2 (Leaf 1) '()) '())) )

(define (tree-to-dot t)
  (cond [(Leaf? t) (leaf (Leaf-val t))]
	[(Tree? t) (string-append (maybe-edge (Tree-val t)
					      (Tree-left t))
				  (maybe-edge (Tree-val t)
					      (Tree-right t)))]))
(define (maybe-edge n m)
  (if (null? m)
      ""
      (string-append
	(edge n (val m))
	(tree-to-dot m))))
(define (leaf n)
  (string-append "\t" (number->string n) ";\n"))

(define (edge n m)
  (string-append "\t" (number->string n) " -- " (number->string m) ";\n"))

(define (dot-graph name t)
  (string-append "graph " name " {\n"
                 (tree-to-dot t)
                 "}\n"))

(define (tree-dot enum count)
  (for ([i (range count)])
    (let ([fname (prefix-zeros i count)])
      (display-to-file
       (dot-graph (number->string i) (decode enum i))
       (string-append fname ".dot")       
       #:exists 'replace))))

(define (prefix-zeros i max)
  (let ([max-length (string-length (number->string max))]
	[is (number->string i)])
    (string-append
     "trees/"
     (if (< (string-length is) max-length)
	 (string-append (make-string (- max-length (string-length is)) #\0)
			is)
	 is))))
