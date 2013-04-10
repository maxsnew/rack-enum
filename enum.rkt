#lang racket
(require rackunit)

;; an Enum a is a struct of < Nat or +Inf, Nat -> a, a -> Nat >
(struct Enum
  (size from to))

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
;; map/enum : Enum a, (a -> b), (b -> a) -> Enum b
(define (map/enum e f inv-f)
  (Enum (size e)
	(compose f (Enum-from e))
	(compose (Enum-to e) inv-f)))

;; to-list : Enum a -> listof a
;; better be finite
(define (to-list e)
  (when (infinite? (size e))
    (error 'too-big))
  (map (Enum-from e)
       (stream->list (in-range (size e)))))

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

;; foldl-enum : Enum a, b, (a,b -> b) -> b
;; better be a finite enum
(define (foldl-enum f id e)
  (foldl f id (to-list e)))

(define (const c)
  (Enum 1
        (λ (n)
          c)
        (λ (x)
          (if (equal? c x)
              0
              (error 'bad-val)))))

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

;; const tests
(test-begin 
 (let [(e (const 17))]
   (check-eq? (decode e 0) 17)
   (check-exn exn:fail? 
              (λ ()
                (decode e 1)))
   (check-eq? (encode e 17) 0)
   (check-exn exn:fail?
              (λ ()
                (encode e 0)))
   (check-bijection? e)))

;; list/enum :: Lisof a -> Gen a
;; input list should not contain duplicates
(define (list/enum l)
  (Enum (length l)
        (λ (n)
          (list-ref l n))
        (λ (x)
          (length (take-while l (λ (y)
                                  (not (eq? x y))))))))

;; take-while : Listof a, (a -> bool) -> Listof a
(define (take-while l pred)
  (cond [(empty? l) (error 'empty)]
        [(not (pred (car l))) '()]
        [else
         (cons (car l)
            (take-while (cdr l) pred))]))

;; list/enum tests
(test-begin
 (let [(e (list/enum '(5 4 1 8)))]
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

(define bools
  (list/enum (list #t #f)))
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

;; sum :: Enum a, Enum b -> Enum (a or b)
(define (sum/enum e1 e2)
  (cond [(not (infinite? (Enum-size e1)))
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
                   (* ((Enum-to e1) x) 2))))]))

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

;; sum tests
(test-begin
 (let [(bool-or-num (sum/enum bools
                              (list/enum '(0 1 2))))
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
(define (prod/enum e1 e2)
  (cond [(not (infinite? (Enum-size e1)))
	 (cond [(not (infinite? (Enum-size e2)))
		(let [(size (* (Enum-size e1)
			       (Enum-size e2)))]
		  (Enum size
			(λ (n) ;; bijection from n -> axb
			   (if (> n size)
			       (error "out of range")
			       (call-with-values
				   (λ ()
				      (quotient/remainder n (Enum-size e1)))
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
		       l))))]))

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

(define bool*bool (prod/enum bools bools))
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

;; dep/enum : Enum a, (a -> Enum b) -> Enum (a,b)
(define (dep/enum e f)
  (cond [(not (infinite? (size e)))
	 (cond [(not (infinite? (size (f (decode e 0)))))
		(let ([s (foldl + 0 (map (compose size f) (to-list e)))])
		  (Enum s
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
					(cdr ab)))))))]
	       ;; finite e, infinite (f e)s
	       [else (Enum +inf.f
			   (λ (n)
			      (call-with-values
				  (λ ()
				     (quotient/remainder n (size e)))
				(λ (q r)
				   (cons (decode e r)
					 (decode (f (decode e r)) q)))))
			   (λ (ab)
			      (+ (* (size e) (encode (f (car ab)) (cdr ab)))
				 (encode e (car ab)))))])]
	(error 'unimpl)))

(define (up-to n)
  (take/enum nats (+ n 1)))

(define n-up
  (dep/enum (list/enum '(0 1 2))
	    up-to))

;; map test
(define (nats+ n)
  (map/enum nats
	    (λ (k)
	       (+ k n))
	    (λ (k)
	       (- k n))))
(define nats+1 (nats+ 1))
(test-begin
 (check-equal? (size nats+1) +inf.f)
 (check-equal? (decode nats+1 0) 1)
 (check-equal? (decode nats+1 1) 2)
 (check-bijection? nats+1))

(define from-n
  (dep/enum (list/enum '(0 1 2))
	    nats+))

(test-begin
 (check-equal? (size n-up) 6)
 (check-equal? (decode n-up 0) (cons 0 0))
 (check-equal? (decode n-up 1) (cons 1 0))
 (check-equal? (decode n-up 2) (cons 1 1))
 (check-equal? (decode n-up 3) (cons 2 0))
 (check-equal? (decode n-up 4) (cons 2 1))
 (check-equal? (decode n-up 5) (cons 2 2))
 (check-bijection? n-up)

 (check-equal? (size from-n) +inf.f)
 (check-equal? (decode from-n 0) (cons 0 0))
 (check-equal? (decode from-n 1) (cons 1 1))
 (check-equal? (decode from-n 2) (cons 2 2))
 (check-equal? (decode from-n 3) (cons 0 1))
 (check-equal? (decode from-n 4) (cons 1 2))
 (check-equal? (decode from-n 5) (cons 2 3))
 (check-equal? (decode from-n 6) (cons 0 2))
 (check-bijection? from-n))


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
