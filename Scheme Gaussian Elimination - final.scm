; Scheme Gaussian Elimination

(define matrix #(1 2 3 4 5 6 7 8 9))
(define n 3)
(define m 3)

(define (index i j n)
  (+ (* n i) j))

(define (print)
  (define (print-iter i)
    (display (subvector matrix (index i 0 n) (index i n n )))
    (display "\n")
	(if (= i (- m 1))
        (display "\n")
        (print-iter (+ i 1))))
  (print-iter 0))

(define (row q n)
  (quotient q n))

(define (col q n)
  (remainder q n))

(define (swap matrix a b)
  (define (swap-iter matrix q end)
  (define tempa (vector-ref matrix q))
  (define l (col q n))
  (define tempb (vector-ref matrix (index b l n)))
    (if (= a (row q n))
        (begin (vector-set! matrix q tempb)
               (vector-set! matrix (index b l n) tempa)))
    (if (= q end)
        matrix
        (swap-iter matrix (+ q 1) end)))
  (swap-iter matrix 0 (index (- m 1) (- n 1) n)))

(define (abs x)
  (if (> x 0)
     x
     (* -1 x)))


(define (maxrow matrix k l)
  (define (maxrow-iter i local-max max-row)
    (if (> (abs (vector-ref matrix (index i l n))) (abs local-max))
        (begin (set! local-max (vector-ref matrix (index i l n)))
               (set! max-row i)))
    (if (= i (- m 1))
        max-row
        (maxrow-iter (+ i 1) local-max max-row)))
  (maxrow-iter 0 (vector-ref matrix (index k l n)) k))

(define (mult matrix factor from-row to-row)
  (define (mult-iter j)
    (define original (vector-ref matrix (index to-row j n)))
    (vector-set! matrix (index to-row j n) (+ original (* (vector-ref matrix (index from-row j n)) factor)))
    (if (= j (- n 1))
           matrix
        (mult-iter (+ j 1))))
  (mult-iter 0))


(define (reduce matrix k l)
  (define (reduce-iter i)
    (if (= i m) 
        matrix
          (begin 
             (mult matrix (* -1 (/ (vector-ref matrix (index i l n)) (vector-ref matrix (index k l n)))) k i)
             (reduce-iter (+ i 1))
           )
        )
    )
  (reduce-iter (+ k 1)))

(define (elimination)
  (define (elimination-iter k l s)
    (define top-row (maxrow matrix k l))
    (if (or (= k (- m 1)) (= l (- n 1)))
        s
        (begin (if (> top-row k)
                   (begin (swap matrix k top-row)
                          (set! s (* s -1))))
               (if (= (vector-ref matrix (index k l n)) 0)
                   (elimination-iter k (+ l 1 s))
                   (begin (reduce matrix k l)
                          (elimination-iter (+ k 1 ) (+ l 1 ) s ))))))
    (elimination-iter 0 0 1))
