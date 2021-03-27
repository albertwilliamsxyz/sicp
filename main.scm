; first implementation of the absolute function
(define (custom-abs-1 x)
  (cond ((> x 0) x)
		((= x 0) 0)
		((< x 0) (- x))))

; second implementation of the absolute function
(define (custom-abs-2 x)
  (cond ((< x 0) (- x))
		(else x)))

; third implementation of the absolute function
(define (custom-abs-3 x)
  (if (< x 0)
	  (- x)
	  x))

; logical operators (and their syntactic forms)
(or #t #f #t)
(and #t #f #t)
(not #t)
(not #f)

; procedure to compute x < y < z
(define (item-in-range-1 x y z)
  (and (> y x) (> z y)))

(define (item-in-range-2 x y z)
  (and (> y x) (< y z)))

; define >= and <= operators (they are called predicates)
(define (custom->=-1 x y)
  (or (> x y) (= x y)))

(define (custom->=-2 x y)
  (not (< x y)))

(define (custom-<=-1 x y)
  (or (< x y) (= x y)))

(define (custom-<=-2 x y)
  (not (> x y)))

; define a procedure that takes three numbers as arguments and returns the sum of the squares of the two large numbers
(define (custom-square x) (* x x))
(define (custom-max x y) (if (> x y) x y))
(define (sum-of-squares x y) (+ (custom-square x) (custom-square y)))

(define (larger-squares-sum x y z)
  (cond 
	((and (> x y) (> x z)) (sum-of-squares x (if (> y z) y z)))
	((and (> y x) (> y z)) (sum-of-squares y (if (> x z) x z)))
	((and (> z x) (> z y)) (sum-of-squares z (if (> x y) x y)))))

; test the evaluation model used by the interpreter
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))
; I've commented the test because it blocks the program if the evaluation model is applicative
; (test 0 (p))

; algorithm to compute the square root of a number
(define (average x y)
  (/ (+ x y) 2))

(define (custom-sqrt x)
  (define (is-guess-good-enough? guess x)
    (< (abs (- (custom-square guess) x)) (/ x 10)))
  (define (improve-guess guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter x guess)
    (if (is-guess-good-enough? guess x)
 	  guess
	  (sqrt-iter x (improve-guess guess x))))
  (sqrt-iter x 1.0))

(define (test-custom-sqrt x)
  (define squared-x (custom-square x))
  (define result (custom-sqrt squared-x))
  (print "Got " result "; Expected " x "; Square " squared-x))
  
(define (test-custom-sqrt-iter steps)
  (cond 
	((> steps 0) 
  	  (test-custom-sqrt steps) 
	  (test-custom-sqrt-iter (- steps 0.0001)))
	(else print "End")))

; factorial
(define (recursive-factorial x)
	(if (= x 1)
			x
			(* x (recursive-factorial (- x 1)))))

(define (iterative-factorial-1 x)
	(define (iterate product counter)
		(if (= counter 0)
				product
				(iterate (* product counter) (- counter 1))))
	(iterate 1 x))

(define (iterative-factorial-2 x)
	(define (iterate product counter max-count)
		(if (> counter max-count)
				product
				(iterate (* counter product)
								 (+ counter 1)
								 max-count)))
	(iterate 1 1 x))

(print (iterative-factorial-2 1))
(print (iterative-factorial-2 2))
(print (iterative-factorial-2 3))
(print (iterative-factorial-2 4))
(print (iterative-factorial-2 5))
(print (iterative-factorial-2 6))
(print (iterative-factorial-2 7))
(print (iterative-factorial-2 8))
(print (iterative-factorial-2 9))
(print (iterative-factorial-2 10))
