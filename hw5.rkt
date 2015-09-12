#lang planet dyoo/simply-scheme:2

;
; Define an accumulate function with an initial value

(define (accumulate f start xs)
  (if (null? xs)
      start
      (accumulate f (f (car xs) start) (cdr xs))))

;
; Exercise 1
; SICP 2.26

(define x (list 1 2 3))
(define y (list 4 5 6))

; (append x y)
; '(1 2 3 4 5 6)

; (cons x y)
; '((1 2 3) 4 5 6)

; (list x y)
; '((1 2 3) (4 5 6))

;
; Exercise 2
; SICP 2.29

; a
(define (make-mobile left right)
  (list left right))

(define (make-branch len structure)
  (list len structure))

(define (left-branch mobile)
   (car mobile))

(define (right-branch mobile)
   (car (cdr mobile)))

;
(define b1 (make-branch 1 1))
(define b2 (make-branch 2 2))
(define b3 (make-branch 3 3))
(define b4 (make-branch 4 4))

(define m1 (make-mobile b1 b3))
(define m2 (make-mobile b2 m1))
(define m3 (make-mobile m2 m1))
(define m4 (make-mobile b1 b1))
;

; b
(define (branch-length branch)
   (car branch))

(define (branch-structure branch)
   (car (cdr branch)))

(define (mobile-weight m)
  (cond ((number? (branch-structure m))
         (branch-structure m))
        ((list? (branch-structure m))
         (+ (mobile-weight (left-branch m))
            (mobile-weight (right-branch m))))))


(eq? (mobile-weight m1) 4)
(eq? (mobile-weight m2) 6)
(eq? (mobile-weight m3) 10)

; c
(define (balanced? m)
  (and (= (* (mobile-weight (left-branch m))
             (branch-length (left-branch m)))
          (* (mobile-weight (right-branch m))
             (branch-length (right-branch m))))
       (balanced? (left-branch m))
       (balanced? (right-branch m))))

;
; Exercise 3

(define (square x) (* x x))

(define (square-tree tree)
  (cond ((number? tree)
         (square tree))
        ((list? tree)
         (map square-tree tree))))

(define (tree-map fn tree)
  (cond ((number? tree)
         (fn tree))
        ((list? tree)
         (map (lambda (x) (tree-map fn x)) tree))))

(define (square-tree-2 tree) (tree-map square tree))

;
; Exercise 4

(define a `((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      `()
      (cons (accumulate op init (map first seqs))
            (accumulate-n op init (map butfirst seqs)))))

(equal? (accumulate-n + 0 `((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
     `(22 26 30))

;
; Exercise 5

(define m `((1 2 3) (4 5 6) (7 8 9)))
(define v `(1 2 3))

(define (dot-product v w)
    (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v))
       m))

(equal? (matrix-*-vector m v) `(14 32 50))

(define (transpose mat)
  (accumulate-n (lambda (x y) (append y (list x))) `() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m-row)
           (map (lambda (n-col)
                  (dot-product m-row n-col))
                cols))
         m)))

(equal? (matrix-*-matrix m m) '((30 36 42) (66 81 96) (102 126 150)))

;
; Exercise 6

(define fold-right accumulate)

(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest))
                  (cdr rest))))
    (iter initial sequence))

;(fold-right / 1 (list 1 2 3))
;(fold-left / 1 (list 1 2 3))
;(fold-right list `() (list 1 2 3))
;(fold-left list `() (list 1 2 3))

; op should be right and left associative

;
; Exercise 7

(define (equal-2? l1 l2)
  (cond ((or (symbol? l1) (number? l2))
         (eq? l1 l2))
        ((null? l1)
         (null? l2))
        ((or (and (pair? l1) (pair? l2))
             (and (list? l1) (list? l2) (= (length l1) (length l2))))
         (and (equal-2? (car l1) (car l2))
              (equal-2? (cdr l1) (cdr l2))))
        (#t #f)))

(eq? (equal-2? 12 12) #t)
(eq? (equal-2? 12 14) #f)
(eq? (equal-2? `a 12) #f)
(eq? (equal-2? `a `a) #t)
(eq? (equal-2? (cons 1 2) (cons 1 2)) #t)
(eq? (equal-2? (list 1 2) (list 1 2)) #t)
(eq? (equal-2? (list 1 2) 3) #t)
(eq? (equal-2? (cons 1 2) (cons 1 3)) #f)
(eq? (equal-2? (cons 1 2) (list 1 2 3)) #f)

;
; Exercise 8

(define (subsets s)
  (if (null? s)
      (list `())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(equal? (subsets `(1 2 3)) `(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)))