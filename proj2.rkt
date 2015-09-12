#lang planet dyoo/simply-scheme:2

; Define the procedure up-split used by corner-split.
; It is similar to right-split, except that it switches the roles
; of below and beside.

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

#|
Right-split and up-split can be expressed as instances of a general splitting
operation. Define a procedure split with the property that evaluating

(define right-split (split beside below))
(define up-split (split below beside))

produces procedures right-split and up-split with the same behaviors as 
the ones already defined.
|#

(define (split splitter pairer)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split splitter pairer) painter (- n 1))))
          (splitter painter (pairer smaller smaller))))))

#|
A two-dimensional vector v running from the origin to a point can be represented
as a pair consisting of an x-coordinate and a y-coordinate. Implement a data 
abstraction for vectors by giving a constructor make-vect and 
corresponding selectors xcor-vect and ycor-vect. In terms of your selectors 
and constructor, implement procedures add-vect, sub-vect, and scale-vect 
that perform the operations vector addition, vector subtraction, and 
multiplying a vector by a scalar:
|#

(define (make-vect xcor ycor)
  (cons xcor ycor))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect scalar v)
  (make-vect (* scalar (xcor-vect v))
             (* scalar (ycor-vect v))))

#|
Here are two possible constructors for frames:

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

For each constructor supply the appropriate selectors to produce an 
implementation for frames.
|#

(define (origin-frame f)
  (list-ref f 0))

(define (edge1-frame f)
  (list-ref f 1))
  
(define (edge2-frame f)
  (list-ref f 2))

(define (origin-frame2 f)
  (car f))

(define (edge1-frame2 f)
  (car (cdr f)))

(define (edge2-frame2 f)
  (cdr (cdr f)))