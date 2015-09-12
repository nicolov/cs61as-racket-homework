#lang planet dyoo/simply-scheme:2

; Exercise 1 - Define fast-expt-iter

(define (even? x)
  (= (remainder x 2) 0))

(define (fast-expt-inner b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-inner (* b b) (/ n 2) a))
        (else (fast-expt-inner b (- n 1) (* b a)))))

(define (fast-expt-iter b n)
  (fast-expt-inner b n 1))

; Exericse 2 - Define phi

(define (phi)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1)
)

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

; Exercise 3 - Define cont-frac

;; Recursive version
(define (cont-frac n d k)
  (define (inner i)
    (cond ((> i k)
           0)
          (else 
           (/ (n i) (+ (d i) (inner (+ i 1)))))))
  
  (inner 1))

;; Iterative version
(define (cont-frac-iter n d k)
  (define (iter res i)
    (cond ((= i 0) res)
          (else (iter (/ (n i)
                         (+ (d i) res))
                      (- i 1)))))
  (iter 0 k))

(define (euler-d i)
  (if (= 0 (remainder (- i 2) 3))
      (* 2 (+ 1(/ (- i 2) 3)))
      1))

(define (e k)
  (cont-frac (lambda (i) 1)
             euler-d
             100))

; Exercise 4 - Define next-perf

(define (sum-of-factors n)
  (define (helper divisor)
    (if (= divisor n)
        0
        (if (= 0 (remainder n divisor))
            (+ divisor (helper (+ 1 divisor)))
            (helper (+ 1 divisor)))))
  (helper 1))

(define (next-perf n)
  (define (inner k)
    (if (= (sum-of-factors k) k)
        k
        (inner (+ 1 k))))
  (inner n))

; Exercise 5 - Explain what happens when the base cases are interchanged.

#|

Your explanation here

|#

; Exercise 6 - Give a formula relating b, n, counter and product in expt-iter.

#|

Formula for expt:

Formula for expt-iter: b^counter * product = b^n

|#
