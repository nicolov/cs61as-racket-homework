#lang planet dyoo/simply-scheme:2
(require racket/stream)
(require racket/promise)

(define (display-line x)
  (newline)
  (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
      empty-stream
      (stream-cons low
                   (stream-enumerate-interval (+ low 1) high))))

;
; Exercise 1
; SICP 3.51

(define (show x)
  (display-line x)
  x)

(define x
  (stream-map show
              (stream-enumerate-interval 0 10)))

(stream-ref x 5)
(stream-ref x 7)

; What is the type of the value of (delay (+ 1 27))?
; a promise

; What is the type of the value of (force (delay (+ 1 27)))?
; 28

;
; Exercise 2

; Can't call stream-cdr on the pair returned by the inner stream-cdr call

;
; Exercise 3

; In the first version, calling `force` will return the full list, whereas in
; the second one, we get a stream.

;
; Exercise 4

(define (num-seq n)
      (stream-cons n
                   (num-seq (if (even? n)
                                (/ n 2)
                                (+ 1 (* n 3))))))

(define s (num-seq 7))

(define (unroll-stream s n)
  (if (< n 0)
      `()
      (cons (stream-first s)
            (unroll-stream (stream-rest s) (- n 1)))))

(equal? (unroll-stream s 16)
        '(7 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1))

(define (seq-length s)
  (define (inner s n)
    (if (= (stream-first s) 1)
        n
        (inner (stream-rest s) (+ n 1))))
  (inner s 1))

(= (seq-length (num-seq 7)) 17)

;
; Exercise 5

; SICP 3.50

(define (stream-map-2 proc . argstreams)
  (if (stream-empty? (car argstreams))
      empty-stream
      (stream-cons
       (apply proc (map stream-first argstreams))
       (apply stream-map-2
              (cons proc (map stream-rest argstreams))))))

(define s1 (stream 1 2 3 4))
(define s2 (stream 5 6 7 8))
(define s3 (stream-map-2 + s1 s2))
(equal? (unroll-stream s3 3) `(6 8 10 12))

; SICP 3.52

(define sum 0)
(define (accum x)
  (set! sum (+ x sum)) 
  sum)
; (trace accum)
(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq))
; sum=0 because no stream has been forced yet
(stream-ref y 7)
; sum=136 because we requested the first 8 even numbers, meaning
; accum was called 16 times with the first 16 numbers:
; sum=16*17/2 = 136

(unroll-stream z 4)
; sum=136 because the `seq` stream has memoized the first 20 elements,
; so that accum is not called again, and thus sum is not touched

; the results would have been different, because the stream would
; have to be "recomputed" again for the call to (display-stream),
; and sum would increase again

; SICP 3.53

; produces a stream of powers of two

(define (add-streams s1 s2) (stream-map-2 + s1 s2))
(define ss (stream-cons 1 (stream-map-2 + ss ss)))
(unroll-stream ss 10)

; SICP 3.54

(define ones (stream-cons 1 ones))
(define integers (stream-cons 1 (add-streams ones integers)))

(define (mul-streams s1 s2) (stream-map-2 * s1 s2))

(define factorials
  (stream-cons 1 (mul-streams factorials
                              (stream-rest integers))))

(equal? (unroll-stream factorials 6) '(1 2 6 24 120 720 5040))

; SICP 3.55

(define (partial-sums s)
  (stream-cons 1 (add-streams (partial-sums s)
                              (stream-rest s))))

(trace partial-sums)

(define a (partial-sums integers))

(equal? (unroll-stream a 5) `(1 3 6 10 15 21))

; SICP 3.56

(define (scale-stream k s)
  (define factor-stream (stream-cons k factor-stream))
  (mul-streams s factor-stream))

(define (merge s1 s2)
  (cond [(stream-empty? s1) s2]
        [(stream-empty? s2) s1]
        (else
         (let [(s1car (stream-first s1))
               (s2car (stream-first s2))]
           (cond ((< s1car s2car)
                  (stream-cons
                   s1car
                   (merge (stream-rest s1) s2)))
                 ((> s1car s2car)
                  (stream-cons
                   s2car
                   (merge s1 (stream-rest s2))))
                 (#t
                  (stream-cons
                   s1car
                   (merge (stream-rest s1)
                          (stream-rest s2)))))))))

(define S (stream-cons 1 (merge (scale-stream 2 S)
                                (merge (scale-stream 3 S)
                                       (scale-stream 5 S)))))

(equal? (unroll-stream S 10)
        '(1 2 3 4 5 6 8 9 10 12 15))

; SICP 3.64

(define (stream-limit s tol)
  (define (inner s last)
    (if (stream-empty? s)
        #f
        (if (< (abs (- last (stream-first s))) tol)
            (stream-first s)
            (inner (stream-rest s) (stream-first s)))))
  (inner (stream-rest s) (stream-first s)))

(define conv (stream 4 7 3 5 4.2 3.1))
(eq? (stream-limit conv 0.5) #f)
(eq? (stream-limit conv 1) 4.2)

; SICP 3.66

; (1, 100): around 200 pairs

; Since there is no base case, the program will just enter in an infinite loop

;
; Exercise 6

(define (fract-stream frac)
  (let [(decimal (/ (car frac) (cadr frac)))]
    (define (inner decimal)
      (stream-cons (floor decimal)
                   (inner (* 10 (- decimal (floor decimal))))))
    (inner (* 10 decimal))))

(define fs (fract-stream `(1 7)))

(eq? (stream-first (stream-rest (stream-rest (fract-stream `(1 7)))))
     2)