#lang planet dyoo/simply-scheme:2

; ex 1

(define (substitute seq from to)
  (if (empty? seq)
      `()
      (sentence (if (equal? (first seq) from)
                    to
                    (first seq))
                (substitute (butfirst seq) from to))))

; ex 3

; g has zero arguments and returns a function that adds 2 to its
; argument

(define (g)
  (lambda (x) (+ x 2)))

; ex 4

(define f1 2) ; 2
(define (f2) 3) ; 3
(define (f3 x) 4) ; 4
(define (f4) (lambda () 5)) ; 5
(define (f5)
  (lambda ()
    (lambda (x) (+ x 3)))) ; 6

; ex 7

(define (make-tester w)
  (lambda (x) (equal? x w)))

; ex 9

(define (square x)
  (* x x))

(define (every f seq)
  (if (empty? seq)
      `()
      (sentence (f (first seq)) (every f (butfirst seq)))))

; ex 10

