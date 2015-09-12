#lang planet dyoo/simply-scheme:2

;
; HW 03
;

; KEEP pattern
(define (numbers sent)
  (cond ((empty? sent) `())
        ((number? (first sent)) (sentence (first sent) (numbers (butfirst sent))))
        (else (numbers (butfirst sent)))))

;; This should output (76 110)
;(numbers '(76 trombones and 110 cornets))

; exercise 1
(define (describe-time x)
  (cond ((> (quotient x 3600) 0)
         (sentence (quotient x 3600) `HOURS (describe-time (remainder x 3600))))
        ((> (quotient x 60) 0)
         (sentence (quotient x 60) `MINUTES (describe-time (remainder x 60))))
        (else
         (sentence x `SECONDS))))

; exercise 2
(define (remove-once what seq)
  (if (empty? seq)
      `()
      (if (equal? what (first seq))
          (butfirst seq)
          (sentence (first seq) (remove-once what (butfirst seq))))))

; exercise 4
(define (loc_helper what seq i)
  (if (empty? seq)
      #f
      (if (equal? (first seq) what)
          i
          (loc_helper what (butfirst seq) (+ 1 i)))))

(define (location what seq) 
  (loc_helper what seq 1))

; exercise 5
(define (initials sent) (if (empty? sent)
                            `()
                            (sentence (first (first sent)) (initials (butfirst sent)))))

; ex 6
(define (copies n what)
  (if (= n 0)
      `()
      (sentence what (copies (- n 1) what))))

; ex 7
(define (base-grade grade)
  (cond ((equal? `A (first grade)) 4)
        ((equal? `B (first grade)) 3)
        ((equal? `C (first grade)) 2)
        ((equal? `D (first grade)) 1)
        ((equal? `E (first grade)) 0)))

(define (grade-modifier grade)
  (if (empty? (butfirst grade))
      0
      (if (equal? `+ (last grade))
          0.33
          -0.33)))

(define (gpa-helper seq tot n)
  (if (empty? seq)
      (/ tot n)
      (gpa-helper (butfirst seq) (+ tot (+ (base-grade (first seq)) 
                                          (grade-modifier (first seq)))) (+ n 1))))

(define (gpa seq)
  (gpa-helper seq 0 0))

; ex 8
(define (expand seq)
  (if (empty? seq)
      `()
      (if (number? (first seq))
          (sentence (copies (first seq) (first (butfirst seq))) 
                    (expand (butfirst (butfirst seq))))
          (sentence (first seq) (expand (butfirst seq))))))

; ex 9
(define (count-helper w n)
  (if (empty? w)
      n
      (count-helper (butfirst w) (+ 1 n))))

(define (count w)
  (count-helper w 0))

; very inefficient
(define (same-shape? seq1 seq2)
  (cond
    ((and (empty? seq1) (empty? seq2)) #t)
    ((not (= (count seq1) (count seq2))) #f)
    ((= (count (first seq1)) (count (first seq2)))
     (same-shape? (butfirst seq1) (butfirst seq2)))
    (else #f)))

; trying to get rid of the second (slow) cond
(define (same-shape-2? seq1 seq2)
  (if
   (and (empty? seq1) (empty? seq2))
   #t
   (if (not (or (empty? seq1) (empty? seq2))) ;if both have elements left
       (if (= (count (first seq1)) (count (first seq2)))
           (same-shape? (butfirst seq1) (butfirst seq2))
           #f)
       #f)))