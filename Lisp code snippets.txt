(define (power x n)
  (if (= n 1)
      x
      (* x (power x (- n 1)))))
(power 2 16)


(define (sum a b)
   (sum-iter a b n))

(define (sum-iter sum a b count)
  (if (> count b)
    sum
     (sum-iter (+ sum (* count a))
               (a)
               (b)
               (+ count 1))))



(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(define (sum a b)
   (sum-iter 0 a b 1))

(define (sum-iter sum a b count)
  (if (> count b)
    sum
     (sum-iter (+ sum (* count a))
               a
               b
               (+ count 1))))
(sum 1 5)


(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (power x n)
  (if (= n 1)
      x
      (* x (power x (- n 1)))))

(define (e-maclaurin x n)
   (maclaurin-iter 0 x n))

(define (maclaurin-iter sum x n)
  (if (= n 0)
    (+ sum 1)
     (maclaurin-iter (+ sum (/ (power x n) (factorial n)))
                     x
                     (- n 1))))

(e-maclaurin 1 1)
(e-maclaurin 1 2)
(e-maclaurin 1 3)
(e-maclaurin 1 4)
(e-maclaurin 1 5)
(e-maclaurin 1 6)
(e-maclaurin 1 7)


(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (power x n)
  (if (= n 1)
      x
      (* x (power x (- n 1)))))

(define (c-maclaurin x n)
   (maclaurin-iter 0 x n 1))

(define (maclaurin-iter sum x n s)
  (if (= n 0)
    (+ sum 1)
     (maclaurin-iter (+ sum (* s (/ (power x (* n 2)) (factorial (* n 2)))))
                     x
                     (- n 1)
                     (* s -1))))

(c-maclaurin .383 10)




(define (day1)
	(display "A partridge in a pear tree"))
(define (day2)
	(display "Two turtle doves"))
(define (day3)
	(display "Three french hens"))
(define (day4)
	(display "Four calling birds"))
(define (day5)
	(display "Five golden rings"))
(define (day6)
	(display "Six geese a-laying"))
(define (day7)
	(display "Seven swans-a-swimming"))
(define (day8)
	(display "Eight maids-a-milking"))
(define (day9)
	(display "Nine ladies dancing"))
(define (day10)
	(display "Ten lords a-leaping"))
(define (day11)
	(display "Eleven pipers piping"))
(define (day12)
  	(display "Twelve drummers drumming"))

(define (gift day)
    (cond ((= day 1) (day1))
          ((= day 2) (day2))
          ((= day 3) (day3))
          ((= day 4) (day4))
          ((= day 5) (day5))
          ((= day 6) (day6))
          ((= day 7) (day7))
          ((= day 8) (day8))
          ((= day 9) (day9))
          ((= day 10) (day10))
          ((= day 11) (day11))
          ((= day 12) (day12))))


(define (day-of-christmas n)
  (define (day-iterator i)
    (if (= i 1 )
        (gift i)
    ((gift i)
     (newline)
     (day-of-christmas (- i 1)))))
  (day-iterator 11))

(day-of-christmas 11)



(define (day1)
	(display "A partridge in a pear tree"))
(define (day2)
	(display "Two turtle doves"))
(define (day3)
	(display "Three french hens"))
(define (day4)
	(display "Four calling birds"))
(define (day5)
	(display "Five golden rings"))
(define (day6)
	(display "Six geese a-laying"))
(define (day7)
	(display "Seven swans-a-swimming"))
(define (day8)
	(display "Eight maids-a-milking"))
(define (day9)
	(display "Nine ladies dancing"))
(define (day10)
	(display "Ten lords a-leaping"))
(define (day11)
	(display "Eleven pipers piping"))
(define (day12)
  	(display "Twelve drummers drumming"))

(define (gift day)
    (cond ((= day 1) (day1))
          ((= day 2) (day2))
          ((= day 3) (day3))
          ((= day 4) (day4))
          ((= day 5) (day5))
          ((= day 6) (day6))
          ((= day 7) (day7))
          ((= day 8) (day8))
          ((= day 9) (day9))
          ((= day 10) (day10))
          ((= day 11) (day11))
          ((= day 12) (day12))))



(define (christmas n)
  (define (christmas-iter i)
    (display i)
    (if (= i 1)
        (newline)
        (christmas-iter (- i 1))))
  (christmas-iter n))



Basic Loop:

(define (square x)
  (* x x))

(define (test n)
  (define (test-iter i)
    (display (square i))
    (display " ")
	(if (= i n)
        (display "done")
        (test-iter (+ i 1))))
  (test-iter 1))



(test 10)

Fibonacci iterator

(define (fib n)
  (define (fib-iter a b i)
	 (display a)
     (display " ")
	(if (= i n)
        (display b)
        (fib-iter b (+ a b) (+ i 1))))
  (fib-iter 1 1 1))



(fib 10)


(define (method n)        ; name of the function
  (define (method-iter i) ; the function iterator, with counter i
	 (display i)          ; the body of the function
     (display " ")        
	(if (= i n)           ; if (stop condition)
        (display "end")   ; last action
        (method-iter (+ i 1))))  ; else -- continue with these parameters
  (method-iter 1))   ; invoke the iterator with initial conditions

(method 5)


Basic Loop

(define (method n)        ; name of the function
  (define (method-iter i) ; the function iterator, with counter i
	 (define (power i) (* i i))   ; the body of the function
     (display (power i))          ; the body of the function
     (display " ")                ; the body of the function
	(if (= i n)           ; if (condition = true)
        (display "end")   ; stop here
        (method-iter (+ i 1))))  ; else continue with these iterated parameters
  (method-iter 1))   ; invoke the iterator with initial conditions

(method 5)



(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(define (power x n)
  (if (<= n 1)
      x
      (* x (power x (- n 1)))))

(define (c-maclaurin x n)
 	(define (maclaurin-iter sum x n s)
  		(if (= n 0)
    	(+ sum 1)
     	(maclaurin-iter (+ sum (* s (/ (power x (* n 2)) (factorial (* n 2)))))
                     x
                     (- n 1)
                     (* s -1))))
    (maclaurin-iter 0 x n 1))

(c-maclaurin 1 10)


(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(define (power x n)
  (if (<= n 1)
      x
      (* x (power x (- n 1)))))

(define (cos x)
 	(define (cos-iter sum x n s)
  		(define nextsum (* s (/ (power x (* n 2)) (factorial (* n 2)))))
      	(if (= n 0)
    		(+ sum 1)
     		(cos-iter (+ sum nextsum)
                     x
                     (- n 1)
                     (* s -1)))
      )
    (cos-iter 0 x 10 1)
 )




(define (cos x)
 	(define (cos-iter sum x n s)
  		(define (power n)
  			(if (<= n 1)
      			x
      			(* x (power (- n 1)))))
         (define (factorial n)
             (if (<= n 1)
                 1
                (* n (factorial (- n 1)))))
      
         (define next-term (* s (/ (power (* n 2)) (factorial (* n 2)))))
            
         (if (= n 0)
    		(+ sum 1)
     		(cos-iter (+ sum next-term)
                     x
                     (- n 1)
                     (* s -1))))
    (cos-iter 0 x 10 1))

(cos 3.14)




Basic Loop K


(define (loop k n)
  
  	(display k)
  	(display " ")
  
 	(if (= k n)
      k
      (loop (+ k 1) n))
  
)

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))


(define (next-s s)
  (* s -1))


(define (numerator k)
  (* (factorial (* 6 k)) (+ 13591409 (* k 545140134))))





(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.01)



(define (cube x) (* x x x))

(define (derivative f a dx)
  (define (difference x)
     (- (f (+ x dx)) (f x)))
  (/ (difference a) dx))

(derivative cube 1 0.01)



(define (cos x)
 	(define (cos-iter sum x n s)
  		(define (power n)
  			(if (<= n 1)
      			x
      			(* x (power (- n 1)))))
         (define (factorial n)
             (if (<= n 1)
                 1
                (* n (factorial (- n 1)))))
      
         (define next-term (* s (/ (power (* n 2)) (factorial (* n 2)))))
            
         (if (= n 0)
    		(+ sum 1)
     		(cos-iter (+ sum next-term)
                     x
                     (- n 1)
                     (* s -1))))
    (cos-iter 0 x 10 1))

(define (derivative f a dx)
  (define (difference x)
     (- (f (+ x dx)) (f x)))
  (/ (difference a) dx))

(derivative cos 1 0.00001)





(define (inc n)
	(+ n 1))

(define (geometric x) x)
    
(define (product term x a next b)
  (if (> a b)
      1
      (* (term x)
         (product term x (next a) next b))))

(define (half-life decay)
  (define (half-life-iter k)
  	(if (>= 0.5 (product geometric decay 1 inc k))
         k
         (half-life-iter (+ k 1))
    ))
  (half-life-iter 1))

(half-life 0.9)


(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

; assuming n is even
(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (add-2h x) (+ x h h))
  (* (+ (f a)
        (* 2 (sum f a       add-2h b))
        (* 4 (sum f (+ a h) add-2h b))
        (f b))
     (/ h 3)))

(define (cube x) (* x x x))

(display (integral cube 0 1 0.01)) (newline)
(display (integral cube 0 1 0.001)) (newline)

(display (integral-simpson cube 0 1.0 100)) (newline)
(display (integral-simpson cube 0 1.0 1000)) (newline)



(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (identity x) x)

(define (inc n) (+ n 1))

(define (factorial n)
  (product identity 1 inc n))

(define (wallis-product n)
  (define (term n)
    (* (/ (* 2 n)
          (- (* 2 n) 1))
       (/ (* 2 n)
          (+ (* 2 n) 1))))
  (product term 1.0 inc n))

(define (Pi n)
  (* 2 (wallis-product n)))

(Pi 1000)




(define (negative? x) (< x 0))
(define (positive? x) (> x 0))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)



(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

(define (average x y)
  (/ (+ x y) 2))

(sqrt 2)



(define (square x)
  (* x x))

(define (mult-complex x y)
  (make-complex (- (* (real x) (real y))
                   (* (imaginary x) (imaginary y)))
                (+ (* (real x) (imaginary y))
                   (+ (real y) (imaginary x)))))

(define (divide-complex x y)
  (make-complex (/ (+ (* (real x) (real y))
                      (* (imaginary x) (imaginary y)))
                   (+ (square (real y)) (square (imaginary y))))
                (/ (- (* (imaginary x) (real y)) (* (real x) (imaginary y)))
                   (+ (square (real y)) (square (imaginary y))))))

(define (add-complex x y)
  (make-complex (+ (real x) (real y))
            (+ (imaginary x) (imaginary y))))

(define (subtract-complex x y)
  (make-complex (- (real x) (real y))
            (- (real x) (real y))))

(define (equal-complex? x y)
  (and (= (real x) (real y))
       (= (imaginary x) (imaginary y))))

(define (make-complex r im) (cons r im))

(define (real x) (car x))

(define (imaginary x) (cdr x))

(define (print-complex x)
  (newline)
  (display (real x))
  (display " + ")
  (display (imaginary x))
  (display "i "))


(define (cube x)
  (* x x x))

(define (square x)
  (* x x))

(define (inverse x)
  (/ 1 x))

(define (derivative g)
  (let ((dx 0.00001))
  (lambda (x)
  (/ (- (g (+ x dx)) (g x)) 
     dx))))

((derivative cube) 1)   ; derivative of x^3 at x = 1
((derivative square) 1) ; derivative of x^2 at x = 1
((derivative cos) 0)    ; derivative of cos(x) at x = 0
((derivative inverse) 1)  ; derivative of 1/x at x = 1


(define (square x)
  (* x x))

(define (mult-complex x y)
  (make-complex (- (* (real x) (real y))
                   (* (imaginary x) (imaginary y)))
                (+ (* (real x) (imaginary y))
                   (* (real y) (imaginary x)))))

(define (divide-complex x y)
  (make-complex (/ (+ (* (real x) (real y))
                      (* (imaginary x) (imaginary y)))
                   (+ (square (real y)) (square (imaginary y))))
                (/ (- (* (imaginary x) (real y)) (* (real x) (imaginary y)))
                   (+ (square (real y)) (square (imaginary y))))))

(define (add-complex x y)
  (make-complex (+ (real x) (real y))
            (+ (imaginary x) (imaginary y))))

(define (subtract-complex x y)
  (make-complex (- (real x) (real y))
            (- (real x) (real y))))

(define (equal-complex? x y)
  (and (= (real x) (real y))
       (= (imaginary x) (imaginary y))))

(define (make-complex r im) (cons r im))

(define (real x) (car x))

(define (imaginary x) (cdr x))

(define (print-complex x)
  (display (real x))
  (display " + ")
  (display (imaginary x))
  (display "i "))

(define cnum1 (make-complex 1 2))

(display "X:")
(print-complex cnum1)
(newline)

(define cnum2 (make-complex 3 4))

(display "Y:")
(print-complex cnum2)
(newline)

(display "X + Y = ")
(print-complex (add-complex cnum1 cnum2))
(newline)
(display "X - Y = ")
(print-complex (subtract-complex cnum1 cnum2))
(newline)
(display "X * Y = ")
(print-complex (mult-complex cnum1 cnum2))
(newline)
(display "X / Y = ")
(print-complex (divide-complex cnum1 cnum2))



FINAL

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define (norm x)
  (sqrt (+ (square (real x))
           (square (imaginary x)))))

(define (mult-complex x y)
  (make-complex (- (* (real x) (real y))
                   (* (imaginary x) (imaginary y)))
                (+ (* (real x) (imaginary y))
                   (* (real y) (imaginary x)))))

(define (divide-complex x y)
  (make-complex (/ (+ (* (real x) (real y))
                      (* (imaginary x) (imaginary y)))
                   (+ (square (real y)) (square (imaginary y))))
                (/ (- (* (imaginary x) (real y)) (* (real x) (imaginary y)))
                   (+ (square (real y)) (square (imaginary y))))))

(define (add-complex x y)
  (make-complex (+ (real x) (real y))
            (+ (imaginary x) (imaginary y))))

(define (subtract-complex x y)
  (make-complex (- (real x) (real y))
            (- (real x) (real y))))

(define (equal-complex? x y)
  (and (= (real x) (real y))
       (= (imaginary x) (imaginary y))))

(define (make-complex r im) (cons r im))

(define (real x) (car x))

(define (imaginary x) (cdr x))

(define (greater? x y)
  (> (norm x) (norm y)))

(define (print-complex x)
  (display (real x))
  (display " + ")
  (display (imaginary x))
  (display "i "))

(define cnum1 (make-complex 1 2))

(display "X:")
(print-complex cnum1)
(newline)

(define cnum2 (make-complex 3 4))

(display "Y:")
(print-complex cnum2)
(newline)

(display "X > Y? ")
(display (greater? cnum1 cnum2))


(define one-through-four (list 1 2 3 4))
one-through-four
(cdr one-through-four)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25))

(list-ref one-through-four 2)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(define odds (list 1 3 5 7))

(length odds)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(append squares odds)

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))





(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (fiblist n)
  (define (fiblist-iter i n a b partial-list)
    (if (> i n)
     partial-list
    (fiblist-iter (+ 1 i) n b (+ a b) (cons partial-list b))))
  (fiblist-iter 0 n 1 1 (cons 0 1)))

(fiblist 10)
    


; iterative loop
(define basic-loop
 (lambda (x)
  (define iterate
   (lambda (n i sum)
    (if (= i n)
      sum
      (iterate n (+ i 1) (+ sum i)))))
  (iterate x 1 0)))

(basic-loop 6)


; recursive loop
(define basic-loop
 (lambda (x)
  (define iterate
   (lambda (n i)
    (if (= i n)
      0
      (+ i (iterate n (+ i 1))))))
  (iterate x 1)))

(basic-loop 6)


(define (plus a b)
  (+ a b))

(define mult
  (lambda (x y)
    (* x y)))

(define square
  (lambda (x)
    (* x x)))

(define factorial
  (lambda (n)
    (define factorial-iter
      (lambda (n product)
         (if (= n 1)
          product
         (factorial-iter (- n 1) (* n product)))))
    (factorial-iter n 1)))

(define sum-of-squares
  (lambda (n)
    (define sum-of-squares-iter
      (lambda (i)
        (if (= i n)
            0
        (+ (* i i) (sum-of-squares-iter (+ i 1))))))
    (sum-of-squares-iter 1)))


(plus 2 3)

(mult 2 3)

(square 3)

(factorial 5)

(sum-of-squares 9)


(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))


(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(deriv '(* x x) 'x)
(deriv '(* x (* x x)) 'x)
(deriv '(* x y) 'x)
(deriv '(+ (* 3 (* x x)) (* 2 x) 5) 'x)


Huffman Encoding

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)



(define reservoir 100)

(define (transfer heat)
  (if (>= reservoir heat)
      (begin (set! reservoir (- reservoir heat))
             reservoir)
      "Reservoir at Equilibrium!"))

(define power 40)
(define cost 0.5)

(define (pump heat)
  (if (and (>= power (* heat cost)) (< reservoir 100))
      (begin (set! reservoir (+ reservoir heat))
             (set! power (- power (* cost heat)))
             (display "power: ")
             (display power)
             (display " reservoir heat: ")
             reservoir)
      "No more power or at capacity!"))

(transfer 40)
(pump 20)
(transfer 30)
(pump 20)
(transfer 20)
(transfer 10)
(pump 20)
(transfer 10)
(pump 20)
(pump 20)
(transfer 10)
(transfer 10)
(transfer 10)
(transfer 10)
(transfer 10)
(transfer 10)
(transfer 10)
(transfer 10)




(define (printmove from to)
  (begin
      (display "move from ")
      (display from)
      (display " to ")
      (display to)
      (newline)
   ))


(define (towers n from to temp)
  (if (= n 1)
      (printmove from to)
      (begin
         (towers (- n 1) from temp to)
         (towers 1 from to temp)
         (towers (- n 1) temp to from)
       )
      )
  )

(towers 4 "P1" "P2" "P3")
