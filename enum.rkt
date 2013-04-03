#lang racket
(require rackunit)

;; an Enum a is a struct of < Nat or +Inf, Nat -> a, a -> Nat >
(struct Enum
  (size from to))

;; decode : Enum a, Nat -> a
(define (decode e n)
  (if (and (< n (Enum-size e))
           (>= n 0))
      ((Enum-from e) n)
      (error 'out-of-range)))

;; encode : Enum a, a -> Nat
(define (encode e a)
  ((Enum-to e) a))

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

;; from-list :: Lisof a -> Gen a
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

;; from-list tests
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
        identity))
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
	   (let* ([k (floor (untri n))]
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

;; prod : Enum a, Enum b -> Enum (a,b)
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
		  (let* ([k (floor (untri n))]
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
;; the inverse of tri (use floor o untri to find largest tri number
;; less than k)
(define (untri k)
  (/ (+ (- 1)
	(sqrt (+ 1 (* 8 k))))
     2))

;; prod tests
(test-begin
 (let ([bool*bool (prod/enum bools bools)]
       [bool*nats (prod/enum bools nats)]
       [nats*bool (prod/enum nats bools)]
       [nats*nats (prod/enum nats nats)]
       [ns-equal? (λ (ns ms)
		     (and (= (car ns)
			     (car ms))
			  (= (cdr ns)
			     (cdr ms))))])
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
   (check-bijection? nats*nats)))
