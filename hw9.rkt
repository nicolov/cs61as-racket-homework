#lang planet dyoo/simply-scheme:2

(require racket/mpair)

#|
Exercise 1. Why did the student get an error?

You can not mutate integers, but only references.

|#

;
; Exercise 2
; Exercise 2a. Fill in the ?? so that the calls produce the desired effect.

(define list1 (mlist (mlist 'a) 'b))
(define list2 (mlist (mlist 'x) 'y))
(define (answer3)
  (set-mcdr! (mcar list2) (mcdr list1))
  (set-mcdr! (mcar list1) (mcar list2)))
(answer3)
;list1 ; Should output ((a x b) b)
;list2 ; Should output ((x b) y)

;
; Exercise 3

; SICP 3.14

; Q: What does mystery do in general?
; A: Reverses lists

; SICP 3.17

(define (count-pairs-broken x)
  (if (not (mpair? x))
      0
      (+ (count-pairs-broken (mcar x))
         (count-pairs-broken (mcdr x))
         1)))

(define test-1 (mcons (mcons 2 `()) (mcons 3 `())))
(= 3 (count-pairs-broken test-1))

(define test-2 (mcons (mcons 2 `()) (mcons 3 `())))
(set-mcdr! (mcar test-2) (mcdr test-2))
(= 4 (count-pairs-broken test-2))

(define test-3 (mcons (mcons '() '()) (mcons '() '())))
(set-mcar! (mcar test-3) (mcdr test-3))
(= 4 (count-pairs-broken test-3))
(set-mcdr! (mcar test-3) (mcdr test-3))
(= 5 (count-pairs-broken test-3))
(set-mcdr! test-3 (mcar test-3))
(= 7 (count-pairs-broken test-3))

(define (count-pairs x) 
  (let [(aux (mlist))]
    (define (inner x) 
      (if (or (not (mpair? x)) (memq x aux)) 
          0 
          (begin 
            (set! aux (cons x aux)) 
            (+ (inner (mcar x)) 
               (inner (mcdr x)) 
               1)))) 
    (inner x)))

(= 3 (count-pairs test-1))
(= 3 (count-pairs test-2))
(= 3 (count-pairs test-3))

; SICP 3.21

(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))
(define (set-front-ptr! queue item) (set-mcar! queue item))
(define (set-rear-ptr! queue item) (set-mcdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (mcons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (mcar (front-ptr queue))))
	  
(define (insert-queue! queue item)
  (let ((new-pair (mcons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-mcdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue)))) 

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (mcdr (front-ptr queue)))
         queue))) 

(define q1 (make-queue))
; (insert-queue! q1 'a)
; (insert-queue! q1 'b)

; The interpreter follows the linked list referenced in the car of the queue,
; but also prints out the last element since it is referenced by the cdr
; of the queue pair

(define (print-queue q)
  (display (mcar q)))
  
; SICP 3.25

(define mcaar (lambda (x) (mcar (mcar x))))

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (mcaar records)) (mcar records))
        (else (assoc key (mcdr records)))))

(define (make-table)
  (mlist `*table*))


(define (lookup-single key table)
  (let [(record (assoc key (mcdr table)))]
    (if record
        (mcdr record)
        #f)))

(define (insert-single! key value table)
  (let [(record (assoc key (mcdr table)))]
    (if record
        (set-mcdr! record value)
        (set-mcdr! table
                   (mcons (mcons key value)
                          (mcdr table))))))

(define (insert! keys value table)
  (if (null? (cdr keys))
      ; last key; just add the record to the current table
      (insert-single! (car keys) value table)
      ; need to recurse further; create the subtable if necessary and go further
      (if (not (assoc (car keys) (mcdr table)))
          (begin (set-mcdr! table
                            (mcons (mcons (car keys) (mlist))
                                   (mcdr table)))
                 (insert! (cdr keys) value (assoc (car keys) (mcdr table))))
          (insert! (cdr keys) value (assoc (car keys) (mcdr table))))))
               
;(trace insert!)

(define (lookup keys table)
  (if (not table)
      #f
      (if (null? (cdr keys))
          (lookup-single (car keys) table)
          (lookup (cdr keys) (assoc (car keys) (mcdr table))))))

;(trace lookup)

(define table (make-table))
(define t (make-table))

(insert! '(a b) 11 t)
(insert! '(b d e) 74 t)

(eq? 11 (lookup '(a b) t))
(eq? #f (lookup '(a c) t))
(eq? 74 (lookup `(b d e) t))

; SICP 3.27

; After the first one, the following computations are not repeated, but fetched
; from the table.

; No, because we would also need to change the two recursive calls within the
; function itself.

;
; Exercise 5

(define (vector-copy src dest i-dest)
  (define (inner src i-src dest i-dest)
    (if (= i-src (vector-length src))
        dest
        (begin (vector-set! dest i-dest (vector-ref src i-src))
               (inner src (+ 1 i-src) dest (+ 1 i-dest)))))
  ;(trace inner)
  (inner src 0 dest i-dest))

(define (vector-append v w)
  (let [(new (make-vector (+ (vector-length v) (vector-length w))))]
    (vector-copy v new 0)
    (vector-copy w new (vector-length v))
    new))

(define v1 (vector 1 2 3 4))
(define v2 (vector 5 6 7))
(define v3 (vector))

(equal? (vector-append v1 v2) `(1 2 3 4 5 6 7))
(equal? (vector-append v3 v1) v1)
(equal? (vector-append v1 v3) v1)

;
; Exercise 6

(define (vector-filter pred? vec)
  (define (do-count-filtered i)
    (cond ((< i 0) 0)
          ((pred? (vector-ref vec i)) (+ 1 (do-count-filtered (- i 1))))
          (else (do-count-filtered (- i 1)))))
  (define (do-filter out n i)
    (cond ((< n 0) out)
          ((pred? (vector-ref vec n))
                  (vector-set! out i (vector-ref vec n))
                  (do-filter out (- n 1) (- i 1)))
          (else (do-filter out (- n 1) i))))
  (let ((filtered-count (do-count-filtered (- (vector-length vec) 1))))
    (let ((dest (make-vector filtered-count)))
      (do-filter dest (- (vector-length vec) 1) (- (vector-length dest) 1)))))

(define (build-range max)
  (define (inner vec i)
    (if (= i max)
        vec
        (begin (vector-set! vec i i)
               (inner vec (+ i 1)))))
  (inner (make-vector max) 1))

(define rng (build-range 10))
(equal? (vector 0 2 4 6 8) (vector-filter (lambda (x) (= (remainder x 2) 0)) rng))

;
; Exercise 7

(define (bubble-sort! v)
  (define (swap! i)
    ; swap the i-th element with the following one
    (let [(tmp (vector-ref v i))]
      (vector-set! v i (vector-ref v (+ i 1)))
      (vector-set! v (+ i 1) tmp)))
  (define (inner i)
    (cond [(= (+ i 1) (vector-length v))
           v]
          [(> (vector-ref v i)
              (vector-ref v (+ i 1)))
           (swap! i)
           (display 'a)
           (inner (+ 0 0))]
          [#t (inner (+ i 1))]))
  (inner 0))

(trace bubble-sort!)

(bubble-sort! (vector 4 5 1 2 3))