#lang planet dyoo/simply-scheme:2

;
; UNIT 1
;

; ex 1

; remove *all* occurrences of a word in a sentence
(define (remove-all what seq)
  (if (empty? seq)
      `()
      (if (equal? (first seq) what)
          (remove-all what (butfirst seq))
          (sentence (first seq) (remove-all what (butfirst seq))))))

; (remove-all `a `(b b c a a b a))

(define (dupls-removed seq)
  (if (empty? seq)
      `()
      (sentence (dupls-removed (remove-all (last seq) (butlast seq)))
                (last seq))))

; ex 2
(define (count-word sent w)
  (define (helper s acc)
    (if (empty? s)
        acc
        (if (equal? (first s) w)
            (helper (butfirst s) (+ 1 acc))
            (helper (butfirst s) acc))))
  (helper sent 0))

; ex 3

; endless loop because the else statement would get evaluated as soon as
; the pigl function is evaluated.

; ex 5

(define (squares seq)
  (if (empty? seq)
      `()
      (sentence (* (first seq) (first seq)) (squares (butfirst seq)))))

; ex 5

; ex 6

(define (ordered? sent)
  (if (empty? (butfirst sent))
      #t
      (if (< (first sent) (first (butfirst sent)))
          (ordered? (butfirst sent))
          #f)))

; ex 7

(define (ends-e sent)
  (if (empty? sent)
      `()
      (if (equal? `e (last (first sent)))
          (sentence (first sent) (ends-e (butfirst sent)))
          (ends-e (butfirst sent)))))

; ex 8

; I would call (or 1 (/ 10 0)) that would only raise an error if or
; is *not* implemented as a special form. A special form would only
; evaluate its first argument and return #t immediately

; 