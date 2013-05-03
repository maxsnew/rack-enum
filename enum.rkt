#lang racket/base
(require racket/math
	 racket/list
	 racket/function
	 rackunit)

(provide Enum
	 size
	 encode
	 decode
	 empty/enum
	 const/enum
	 from-list/enum
	 sum/enum
	 prod/enum
	 dep/enum
	 dep2/enum ;; doesn't require size
	 map/enum
	 filter/enum ;; very bad, only use for small enums
	 except/enum 
	 thunk/enum
	 listof/enum
	 list/enum
	 
	 to-list
	 take/enum
	 drop/enum
	 foldl-enum
	 display-enum

	 nats
	 range/enum
	 nats+/enum

	 check-bijection?)

;; an Enum a is a struct of < Nat or +Inf, Nat -> a, a -> Nat >
(struct Enum
	(size from to)
	#:prefab)

;; size : Enum a -> Nat or +Inf
(define (size e)
  (Enum-size e))

;; decode : Enum a, Nat -> a
(define (decode e n)
  (if (and (< n (Enum-size e))
           (>= n 0))
      ((Enum-from e) n)
      (error 'out-of-range)))

;; encode : Enum a, a -> Nat
(define (encode e a)
  ((Enum-to e) a))

;; Helper functions
;; map/enum : (a -> b), (b -> a), Enum a -> Enum b
(define (map/enum f inv-f e)
  (Enum (size e)
	(compose f (Enum-from e))
	(compose (Enum-to e) inv-f)))




;; filter/enum : Enum a, (a -> bool) -> Enum a
;; size won't be accurate!
;; encode is not accurate right now!
(define (filter/enum e p)
  (Enum (size e)
	(λ (n)
	   (let loop ([i 0]
		      [seen 0])
	     (let ([a (decode e i)])
	       (if (p a)
		   (if (= seen n)
		       a
		       (loop (+ i 1) (+ seen 1)))
		   (loop (+ i 1) seen)))))
	(λ (x) (encode e x))))

;; except/enum : Enum a, a -> Enum a
(define (except/enum e a)
  (unless (> (size e) 0)
    (error 'empty-enum))
  (let ([m (encode e a)])
    (Enum (- (size e) 1)
	  (λ (n)
	     (if (< n m)
		 (decode e n)
		 (decode e (+ n 1))))
	  (λ (x)
	     (let ([n (encode e x)])
	       (cond [(< n m) n]
		     [(> n m) (- n 1)]
		     [else (error 'excepted)]))))))

;; to-list : Enum a -> listof a
;; better be finite
(define (to-list e)
  (when (infinite? (size e))
    (error 'too-big))
  (map (Enum-from e)
       (build-list (size e)
		   identity)))

;; take/enum : Enum a, Nat -> Enum a
;; returns an enum of the first n parts of e
;; n must be less than or equal to size e
(define (take/enum e n)
  (unless (or (infinite? (size e))
	      (<= n (size e)))
    (error 'too-big))
  (Enum n
	(λ (k)
	   (unless (< k n)
	     (error 'out-of-range))
	   (decode e k))
	(λ (x)
	   (let ([k (encode e x)])
	     (unless (< k n)
	       (error 'out-of-range))
	     k))))

;; drop/enum : Enum a, Nat -> Enum a
(define (drop/enum e n)
  (unless (or (infinite? (size e))
	      (<= n (size e)))
    (error 'too-big))
  (Enum (- (size e) n)
	(λ (m)
	   (decode e (+ n m)))
	(λ (x)
	   (- (encode e x) n))))

;; foldl-enum : Enum a, b, (a,b -> b) -> b
;; better be a finite enum
(define (foldl-enum f id e)
  (foldl f id (to-list e)))

;; display-enum : Enum a, Nat -> void
(define (display-enum e n)
  (for ([i (range n)])
    (display (decode e i))
    (newline) (newline)))

(define empty/enum
  (Enum 0
	(λ (n)
	   (error 'empty))
	(λ (x)
	   (error 'empty))))

(define (const/enum c)
  (Enum 1
        (λ (n)
          c)
        (λ (x)
          (if (equal? c x)
              0
              (error 'bad-val)))))

;; from-list/enum :: Listof a -> Gen a
;; input list should not contain duplicates
(define (from-list/enum l)
  (if (empty? l)
      empty/enum
      (Enum (length l)
	    (λ (n)
	       (list-ref l n))
	    (λ (x)
	       (length (take-while l (λ (y)
					(not (eq? x y)))))))))

;; take-while : Listof a, (a -> bool) -> Listof a
(define (take-while l pred)
  (cond [(empty? l) (error 'empty)]
        [(not (pred (car l))) '()]
        [else
         (cons (car l)
            (take-while (cdr l) pred))]))

(define bools
  (from-list/enum (list #t #f)))
(define nats
  (Enum +inf.f
        identity
        (λ (n)
	   (unless (>= n 0)
	     (error 'out-of-range))
	   n)))
(define ints
  (Enum +inf.f
        (λ (n)
          (if (even? n)
              (* -1 (/ n 2))
              (/ (+ n 1) 2)))
        (λ (n)
          (if (> n 0)
              (- (* 2 n) 1)
              (* 2 (abs n))))))

;; sum :: Enum a, Enum b -> Enum (a or b)
(define sum/enum
  (case-lambda
    [(e) e]
    [(e1 e2)
     (cond
      [(= 0 (size e1)) e2]
      [(= 0 (size e2)) e1]
      [(not (infinite? (Enum-size e1)))
       (Enum (+ (Enum-size e1)
		(Enum-size e2))
	     (λ (n)
		(if (< n (Enum-size e1))
		    ((Enum-from e1) n)
		    ((Enum-from e2) (- n (Enum-size e1)))))
	     (λ (x)
		(with-handlers ([exn:fail? (λ (_)
					      (+ (Enum-size e1)
						 ((Enum-to e2) x)))])
		  ((Enum-to e1) x))))]
      [(not (infinite? (Enum-size e2)))
       (sum/enum e2 e1)]
      [else ;; both infinite, interleave them
       (Enum +inf.f
	     (λ (n)
		(if (even? n)
		    ((Enum-from e1) (/ n 2))
		    ((Enum-from e2) (/ (- n 1) 2))))
	     (λ (x)
		(with-handlers ([exn:fail? 
				 (λ (_)
				    (+  (* ((Enum-to e2) x) 2)
					1))])
		  (* ((Enum-to e1) x) 2))))])]
    [(a b c . rest)
     (sum/enum a (apply sum/enum b c rest))]))

(define odds
  (Enum +inf.f
        (λ (n)
          (+ (* 2 n) 1))
        (λ (n)
          (if (and (not (zero? (modulo n 2)))
                   (>= n 0))
              (/ (- n 1) 2)
              (error 'odd)))))

(define evens
  (Enum +inf.f
        (λ (n)
          (* 2 n))
        (λ (n)
          (if (and (zero? (modulo n 2))
                   (>= n 0))
              (/ n 2)
              (error 'even)))))

(define n*n
  (Enum +inf.f
	(λ (n)
	   ;; calculate the k for which (tri k) is the greatest
	   ;; triangle number <= n
	   (let* ([k (floor-untri n)]
		  [t (tri k)]
		  [l (- n t)]
		  [m (- k l)])
	     (cons l m)))
	(λ (ns)
	   (unless (pair? ns)
	     (error "not a list"))
	   (let ([l (car ns)]
		 [m (cdr ns)])
	     (+ (/ (* (+ l m) (+ l m 1))
		   2)
		l))) ;; (n,m) -> (n+m)(n+m+1)/2 + n
	))

;; prod/enum : Enum a, Enum b -> Enum (a,b)
(define prod/enum
  (case-lambda
    [(e) e]
    [(e1 e2)
     (cond [(or (= 0 (size e1))
		(= 0 (size e2))) empty/enum]
	   [(not (infinite? (Enum-size e1)))
	    (cond [(not (infinite? (Enum-size e2)))
		   (let [(size (* (Enum-size e1)
				  (Enum-size e2)))]
		     (Enum size
			   (λ (n) ;; bijection from n -> axb
			      (if (> n size)
				  (error "out of range")
				  (call-with-values
				      (λ ()
					 (quotient/remainder n (Enum-size e2)))
				    (λ (q r)
				       (cons ((Enum-from e1) q)
					     ((Enum-from e2) r))))))
			   (λ (xs)
			      (unless (pair? xs)
				(error "not a pair"))
			      (+ (* (Enum-size e1)
				    ((Enum-to e1) (car xs)))
				 ((Enum-to e2) (cdr xs))))))]
		  [else
		   (Enum +inf.f
			 (λ (n)
			    (call-with-values
				(λ ()
				   (quotient/remainder n (Enum-size e1)))
			      (λ (q r)
				 (cons ((Enum-from e1) r)
				       ((Enum-from e2) q)))))
			 (λ (xs)
			    (unless (pair? xs)
			      (error "not a pair"))
			    (+ ((Enum-to e1) (car xs))
			       (* (Enum-size e1)
				  ((Enum-to e2) (cdr xs))))))])]
	   [(not (infinite? (Enum-size e2)))
	    (Enum +inf.f
		  (λ (n)
		     (call-with-values
			 (λ ()
			    (quotient/remainder n (Enum-size e2)))
		       (λ (q r)
			  (cons ((Enum-from e1) q)
				((Enum-from e2) r)))))
		  (λ (xs)
		     (unless (pair? xs)
		       (error "not a pair"))
		     (+ (* (Enum-size e2)
			   ((Enum-to e1) (car xs)))
			((Enum-to e2) (cdr xs)))))]
	   [else
	    (Enum (* (Enum-size e1)
		     (Enum-size e2))
		  (λ (n)
		     (let* ([k (floor-untri n)]
			    [t (tri k)]
			    [l (- n t)]
			    [m (- k l)])
		       (cons ((Enum-from e1) l)
			     ((Enum-from e2) m))))
		  (λ (xs) ;; bijection from nxn -> n, inverse of previous
		     ;; (n,m) -> (n+m)(n+m+1)/2 + n
		     (unless (pair? xs)
		       (error "not a pair"))
		     (let ([l ((Enum-to e1) (car xs))]
			   [m ((Enum-to e2) (cdr xs))])
		       (+ (/ (* (+ l m) (+ l m 1))
			     2)
			  l))))])]
    [(a b c . rest)
     (prod/enum a (apply prod/enum b c rest))]))

;; the nth triangle number
(define (tri n)
  (/ (* n (+ n 1))
     2))

;; the floor of the inverse of tri
;; returns the largest triangle number less than k
;; always returns an integer
(define (floor-untri k)
  (let ([n (integer-sqrt (+ 1 (* 8 k)))])
    (/ (- n 
          (if (even? n)
              2
              1))
       2)))


;; dep/enum : Enum a (a -> Enum b) -> Enum (a,b)
(define (dep/enum e f)
  (cond [(= 0 (size e)) empty/enum]
	[(not (infinite? (size (f (decode e 0)))))
	 (Enum (if (infinite? (size e))
		   +inf.f
		   (foldl + 0 (map (compose size f) (to-list e))))
	       (λ (n) ;; n -> axb
		  (let loop ([ei 0]
			     [seen 0])
		    (let* ([a (decode e ei)]
			   [e2 (f a)])
		      (if (< (- n seen)
			     (size e2))
			  (cons a (decode e2 (- n seen)))
			  (loop (+ ei 1)
				(+ seen (size e2)))))))
	       (λ (ab) ;; axb -> n
		  (let ([ai (encode e (car ab))])
		    (+ (let loop ([i 0]
				  [sum 0])
			 (if (>= i ai)
			     sum
			     (loop (+ i 1)
				   (+ sum
				      (size (f (decode e i)))))))
		       (encode (f (car ab))
			       (cdr ab))))))]
	[(not (infinite? (size e)))
	 (Enum +inf.f
	       (λ (n)
		  (call-with-values
		      (λ ()
			 (quotient/remainder n (size e)))
		    (λ (q r)
		       (cons (decode e r)
			     (decode (f (decode e r)) q)))))
	       (λ (ab)
		  (+ (* (size e) (encode (f (car ab)) (cdr ab)))
		     (encode e (car ab)))))]
	[else ;; both infinite, same as prod/enum
	 (Enum +inf.f               
	       (λ (n)
		  (let* ([k (floor-untri n)]
			 [t (tri k)]
			 [l (- n t)]
			 [m (- k l)]
			 [a (decode e l)])
		    (cons a
			  (decode (f a) m))))
	       (λ (xs) ;; bijection from nxn -> n, inverse of previous
		  ;; (n,m) -> (n+m)(n+m+1)/2 + n
		  (unless (pair? xs)
		    (error "not a pair"))
		  (let ([l (encode e (car xs))]
			[m (encode (f (car xs)) (cdr xs))])
		    (+ (/ (* (+ l m) (+ l m 1))
			  2)
		       l))))]))

;; dep2 : Enum a (a -> Enum b) -> Enum (a,b)
(define (dep2/enum e f)
  (cond [(= 0 (size e)) empty/enum]
	[(not (infinite? (size (f (decode e 0)))))
	 ;; memoize tab : boxof (hash nat -o> (nat . nat))
	 ;; maps an index into the dep/enum to the 2 indices that we need
	 (let ([tab (box (hash))])
	   (Enum (if (infinite? (size e))
		     +inf.f
		     (foldl + 0 (map (compose size f) (to-list e))))
		 (λ (n) ;; n -> axb
		    (call-with-values
			(λ ()
			   (letrec
			       ;; go : nat -> nat nat
			       ([go
				 (λ (n)
				    (cond [(hash-has-key? (unbox tab) n)
					   (let ([ij (hash-ref (unbox tab) n)])
					     (values (car ij) (cdr ij)))]
					  [(= n 0) ;; find the first element
					   (find 0 0 0)]
					  [else ;; recurse
					   (call-with-values
					       (λ () (go (- n 1)))
					     (λ (ai bi)
						(find ai (- n bi 1) n)))]))]
				;; find : nat nat nat -> nat
				[find
				 ;; start is our starting eindex
				 ;; seen is how many we've already seen
				 (λ (start seen n)
				    (let loop ([ai start]
					       [seen seen])
				      (let* ([a (decode e ai)]
					     [bs (f a)])
					(cond [(< (- n seen)
						  (size bs))
					       (let ([bi (- n seen)])
						 (begin
						   (set-box! tab
							     (hash-set (unbox tab)
								       n
								       (cons ai bi)))
						   (values ai bi)))]
					      [else
					       (loop (+ ai 1)
						     (+ seen (size bs)))]))))])
			     (go n)))
		      (λ (ai bi)
			 (let ([a (decode e ai)])
			   (cons a
				 (decode (f a) bi))))))
		 ;; todo: memoize encode
		 (λ (ab) ;; axb -> n
		    (let ([ai (encode e (car ab))])
		      (+ (let loop ([i 0]
				    [sum 0])
			   (if (>= i ai)
			       sum
			       (loop (+ i 1)
				     (+ sum
					(size (f (decode e i)))))))
			 (encode (f (car ab))
				 (cdr ab)))))))]
	[else ;; both infinite, same as prod/enum
	 (dep/enum e f)]))



;; more utility enums
;; nats of course
(define (range/enum low high)
  (cond [(> low high) (error 'bad-range)]
	[(infinite? high)
	 (if (infinite? low)
	     ints
	     (map/enum
	      (λ (n)
		 (+ n low))
	      (λ (n)
		 (- n low))
	      nats))]
	[(infinite? low)
	 (map/enum
	  (λ (n)
	     (- high n))
	  (λ (n)
	     (+ high n))
	  nats)]
	[else
	 (map/enum (λ (n) (+ n low))
		   (λ (n) (- n low))
		   (take/enum nats (+ 1 (- high low))))]))

;; thunk/enum : Nat or +-Inf, ( -> Enum a) -> Enum a
(define (thunk/enum s thunk)
  (Enum s
	(λ (n)
	   (decode (thunk) n))
	(λ (x)
	   (encode (thunk) x))))

;; listof/enum : Enum a -> Enum (listof a)
(define (listof/enum e)
  (thunk/enum
   (if (= 0 (size e))
       0
       +inf.f)
   (λ ()
      (sum/enum
       (const/enum '())
       (prod/enum e (listof/enum e))))))

;; list/enum : listof (Enum any) -> Enum (listof any)
(define (list/enum es)
  (apply prod/enum (append es `(,(const/enum '())))))

;;
(define confidence 1000)
(define nums (build-list confidence identity))
(define-simple-check (check-bijection? e)
  (let ([nums (build-list (if (<= (Enum-size e) confidence)
                              (Enum-size e)
                              confidence)
                          identity)])
    (andmap =
	    nums
	    (map (λ (n)
		    (encode e (decode e n)))
		 nums))))

;; const/enum tests
(let [(e (const/enum 17))]
  (test-begin
   (check-eq? (decode e 0) 17)
   (check-exn exn:fail? 
              (λ ()
		 (decode e 1)))
   (check-eq? (encode e 17) 0)
   (check-exn exn:fail?
              (λ ()
		 (encode e 0)))
   (check-bijection? e)))

;; from-list/enum tests
(let [(e (from-list/enum '(5 4 1 8)))]
  (test-begin
   (check-eq? (decode e 0) 5)
   (check-eq? (decode e 3) 8)
   (check-exn exn:fail?
	      (λ () (decode e 4)))
   (check-eq? (encode e 5) 0)
   (check-eq? (encode e 8) 3)
   (check-exn exn:fail?
              (λ ()
		 (encode e 17)))
   (check-bijection? e)))

;; map test
(define (nats+/enum n)
  (map/enum (λ (k)
	       (+ k n))
	    (λ (k)
	       (- k n))
	    nats))
(define nats+1 (nats+/enum 1))

(test-begin
 (check-equal? (size nats+1) +inf.f)
 (check-equal? (decode nats+1 0) 1)
 (check-equal? (decode nats+1 1) 2)
 (check-bijection? nats+1))
;; encode check
(test-begin
 (check-exn exn:fail?
            (λ ()
              (decode nats -1))))

;; ints checks
(test-begin
 (check-eq? (decode ints 0) 0) ; 0 -> 0
 (check-eq? (decode ints 1) 1) ; 1 -> 1
 (check-eq? (decode ints 2) -1); 2 -> 1
 (check-eq? (encode ints 0) 0)
 (check-eq? (encode ints 1) 1)
 (check-eq? (encode ints -1) 2)
 (check-bijection? ints)) ; -1 -> 2, -3 -> 4

;; sum tests
(test-begin
 (let [(bool-or-num (sum/enum bools
                              (from-list/enum '(0 1 2))))
       (bool-or-nat (sum/enum bools
                              nats))
       (nat-or-bool (sum/enum nats
                              bools))
       (odd-or-even (sum/enum evens
                              odds))]
   (check-equal? (Enum-size bool-or-num)
                 5)
   (check-equal? (decode bool-or-num 0) #t)
   (check-equal? (decode bool-or-num 1) #f)
   (check-equal? (decode bool-or-num 2) 0)
   (check-exn exn:fail?
              (λ ()
		 (decode bool-or-num 5)))
   (check-equal? (encode bool-or-num #f) 1)
   (check-equal? (encode bool-or-num 2) 4)
   (check-bijection? bool-or-num)
   
   (check-equal? (Enum-size bool-or-nat)
                 +inf.f)
   (check-equal? (decode bool-or-nat 0) #t)
   (check-equal? (decode bool-or-nat 2) 0)
   (check-bijection? bool-or-nat)
   
   (check-equal? (encode bool-or-num #f) 1)
   (check-equal? (encode bool-or-num 2) 4)

   (check-equal? (Enum-size odd-or-even)
                 +inf.f)
   (check-equal? (decode odd-or-even 0) 0)
   (check-equal? (decode odd-or-even 1) 1)
   (check-equal? (decode odd-or-even 2) 2)
   (check-exn exn:fail?
              (λ ()
		 (decode odd-or-even -1)))
   (check-equal? (encode odd-or-even 0) 0)   
   (check-equal? (encode odd-or-even 1) 1)
   (check-equal? (encode odd-or-even 2) 2)
   (check-equal? (encode odd-or-even 3) 3)
   (check-bijection? odd-or-even)))

;; prod/enum tests
(define bool*bool (prod/enum bools bools))
(define 1*b (prod/enum (const/enum 1) bools))
(define bool*nats (prod/enum bools nats))
(define nats*bool (prod/enum nats bools))
(define nats*nats (prod/enum nats nats))
(define ns-equal? (λ (ns ms)
	      (and (= (car ns)
		      (car ms))
		   (= (cdr ns)
		      (cdr ms)))))

;; prod tests
(test-begin

 (check-equal? (size 1*b) 2)
 (check-equal? (decode 1*b 0) (cons 1 #t))
 (check-equal? (decode 1*b 1) (cons 1 #f))
 (check-bijection? 1*b)
 (check-equal? (Enum-size bool*bool) 4)
 (check-equal? (decode bool*bool 0)
	       (cons #t #t))
 (check-equal? (decode bool*bool 1)
	       (cons #t #f))
 (check-equal? (decode bool*bool 2)
	       (cons #f #t))
 (check-equal? (decode bool*bool 3)
	       (cons #f #f))
 (check-bijection? bool*bool)

 (check-equal? (Enum-size bool*nats) +inf.f)
 (check-equal? (decode bool*nats 0)
	       (cons #t 0))
 (check-equal? (decode bool*nats 1)
	       (cons #f 0))
 (check-equal? (decode bool*nats 2)
	       (cons #t 1))
 (check-equal? (decode bool*nats 3)
	       (cons #f 1))
 (check-bijection? bool*nats)

 (check-equal? (Enum-size nats*bool) +inf.f)
 (check-equal? (decode nats*bool 0)
	       (cons 0 #t))
 (check-equal? (decode nats*bool 1)
	       (cons 0 #f))
 (check-equal? (decode nats*bool 2)
	       (cons 1 #t))
 (check-equal? (decode nats*bool 3)
	       (cons 1 #f))
 (check-bijection? nats*bool)

 (check-equal? (Enum-size nats*nats) +inf.f)
 (check ns-equal?
	(decode nats*nats 0)
	(cons 0 0))
 (check ns-equal?
	(decode nats*nats 1)
	(cons 0 1))
 (check ns-equal?
	(decode nats*nats 2)
	(cons 1 0))
 (check ns-equal?
	(decode nats*nats 3)
	(cons 0 2))
 (check ns-equal?
	(decode nats*nats 4)
	(cons 1 1))
 (check-bijection? nats*nats))


;; dep/enum tests
(define (up-to n)
  (take/enum nats (+ n 1)))

(define 3-up
  (dep/enum
   (from-list/enum '(0 1 2))
   up-to))

(define from-3
  (dep/enum
   (from-list/enum '(0 1 2))
   nats+/enum))

(define nats-to
  (dep/enum nats up-to))

(define nats-up
  (dep/enum nats nats+/enum))

(test-begin
 (check-equal? (size 3-up) 6)
 (check-equal? (decode 3-up 0) (cons 0 0))
 (check-equal? (decode 3-up 1) (cons 1 0))
 (check-equal? (decode 3-up 2) (cons 1 1))
 (check-equal? (decode 3-up 3) (cons 2 0))
 (check-equal? (decode 3-up 4) (cons 2 1))
 (check-equal? (decode 3-up 5) (cons 2 2))
 (check-bijection? 3-up)

 (check-equal? (size from-3) +inf.f)
 (check-equal? (decode from-3 0) (cons 0 0))
 (check-equal? (decode from-3 1) (cons 1 1))
 (check-equal? (decode from-3 2) (cons 2 2))
 (check-equal? (decode from-3 3) (cons 0 1))
 (check-equal? (decode from-3 4) (cons 1 2))
 (check-equal? (decode from-3 5) (cons 2 3))
 (check-equal? (decode from-3 6) (cons 0 2))
 (check-bijection? from-3)

 (check-equal? (size nats-to) +inf.f)
 (check-equal? (decode nats-to 0) (cons 0 0))
 (check-equal? (decode nats-to 1) (cons 1 0))
 (check-equal? (decode nats-to 2) (cons 1 1))
 (check-equal? (decode nats-to 3) (cons 2 0))
 (check-equal? (decode nats-to 4) (cons 2 1))
 (check-equal? (decode nats-to 5) (cons 2 2))
 (check-equal? (decode nats-to 6) (cons 3 0))
 (check-bijection? nats-to)

 (check-equal? (size nats-up) +inf.f)
 (check-equal? (decode nats-up 0) (cons 0 0))
 (check-equal? (decode nats-up 1) (cons 0 1))
 (check-equal? (decode nats-up 2) (cons 1 1))
 (check-equal? (decode nats-up 3) (cons 0 2))
 (check-equal? (decode nats-up 4) (cons 1 2))
 (check-equal? (decode nats-up 5) (cons 2 2))
 (check-equal? (decode nats-up 6) (cons 0 3))
 (check-equal? (decode nats-up 7) (cons 1 3))

 (check-bijection? nats-up))

;; dep2/enum tests
;; same as dep unless the right side is finite
(define 3-up-2
  (dep2/enum
   (from-list/enum '(0 1 2))
   up-to))

(define nats-to-2
  (dep2/enum nats up-to))


(test-begin
 (check-equal? (size 3-up-2) 6)
 (check-equal? (decode 3-up-2 0) (cons 0 0))
 (check-equal? (decode 3-up-2 1) (cons 1 0))
 (check-equal? (decode 3-up-2 2) (cons 1 1))
 (check-equal? (decode 3-up-2 3) (cons 2 0))
 (check-equal? (decode 3-up-2 4) (cons 2 1))
 (check-equal? (decode 3-up-2 5) (cons 2 2))
 (check-bijection? 3-up-2)

 (check-equal? (size nats-to-2) +inf.f)
 (check-equal? (decode nats-to-2 0) (cons 0 0))
 (check-equal? (decode nats-to-2 1) (cons 1 0))
 (check-equal? (decode nats-to-2 2) (cons 1 1))
 (check-equal? (decode nats-to-2 3) (cons 2 0))
 (check-equal? (decode nats-to-2 4) (cons 2 1))
 (check-equal? (decode nats-to-2 5) (cons 2 2))
 (check-equal? (decode nats-to-2 6) (cons 3 0))
 (check-bijection? nats-to-2)
 )


;; take/enum test
(define to-2 (up-to 2))
(test-begin
 (check-equal? (size to-2) 3)
 (check-equal? (decode to-2 0) 0)
 (check-equal? (decode to-2 1) 1)
 (check-equal? (decode to-2 2) 2)
 (check-bijection? to-2))

;; to-list, foldl test
(test-begin
 (check-equal? (to-list (up-to 3))
	       '(0 1 2 3))
 (check-equal? (foldl-enum cons '() (up-to 3))
	       '(3 2 1 0)))

