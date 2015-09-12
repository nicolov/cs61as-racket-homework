#lang planet dyoo/simply-scheme:2

; Exercise 1

; SICP 2.7 - Define upper-bound and lower-bound

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

; SICP 2.8 - Define sub-interval

(define (sub-interval x y)
  (make-interval (+ (lower-bound x) (upper-bound y))
                 (+ (upper-bound x) (lower-bound y))))

; SICP 2.10 - Modify div-interval

(define (spans-zero? x)
  (and (<= (lower-bound x) 0)
       (>= (upper-bound x) 0)))

(define (div-interval x y)
  (if (spans-zero? y)
      (error "Division by zero")
      (mul-interval x 
                    (make-interval (/ 1 (upper-bound y))
                                   (/ 1 (lower-bound y))))))

; SICP 2.12 - Define make-center-percent and percent

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c pct)
  (make-center-width c (* (/ pct 100) c)))

; SICP 2.17 - Define last-pair

(define (last-pair l)
  (if (= (length l) 1)
      l
      (last-pair (cdr l))))

; SICP 2.20 - Define same-parity

; SICP 2.22 - Write your explanation in the comment block:
#|

By using cons this way, the latest element that was fetched from
the `items` list ends up at the front of the resulting list. So
the order is inverted.
In the second case, cons is used to build a pair whose first element
is a list and the second a number. So you end up with a strange data
structure..

|#


; Exercise 2 - Define substitute

(define (substitute ll ow nw)
  (cond ((null? ll)
         `())
        ((not (pair? ll))
         (if (equal? ll ow)
             nw
             ll))
        (else (cons (substitute (car ll) ow nw)
                    (substitute (cdr ll) ow nw)))))

; Exercise 3 - Define substitute2

(define zip (lambda (l1 l2) (map list l1 l2)))

; returns the mapped element if it exists or the original one
(define (get-mapped x mapping)
  (cond ((null? mapping)
         x)
        ((equal? x (caar mapping))
         (car (cdr (car mapping))))
        (else (get-mapped x (cdr mapping)))))

(define (substitute2 ll origs subs)
  (let ((mapping (zip origs subs)))
    (cond ((null? ll)
           `())
          ((not (pair? ll))
           (get-mapped ll mapping))
          (else (cons (substitute2 (car ll) origs subs)
                      (substitute2 (cdr ll) origs subs))))))
           