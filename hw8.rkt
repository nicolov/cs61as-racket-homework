#lang planet dyoo/simply-scheme:2

; SICP 3.3, 3.4 - Modify make-password-account to make it generate
; password-protected accounts.
; Also, if an incorrect password is given 7 times consecutively, you
; should say (call-the-cops).
; Note: In the case of a wrong password, you should return the string
; "Incorrect password".  Do not use display or print or error.

(define (call-the-cops)
  (error "Calling the cops.."))

(define (make-password-account balance)
  (define mistakes 0)
  (define passwords `(secret-password))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (add-password password)
    (set! passwords (cons password passwords)))
  (define (dispatch password m)
    (if (member password passwords)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m `add-password) add-password)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (begin (set! mistakes (+ mistakes 1))
               (if (> mistakes 7)
                   (call-the-cops)
                   (lambda (x) "Incorrect password")))))
  dispatch)

(define acc (make-password-account 100))

(eq? ((acc 'secret-passord 'withdraw) 10) "Incorrect password")
(eq? ((acc 'secret-password 'withdraw) 10) 90)

; SICP 3.7 - Define make-joint.
; You may want to modify make-password-account, but you shouldn't
; remove any of its previous functionality.

(define (make-joint acc old-password new-password)
  ((acc old-password `add-password) new-password))

(eq? ((acc 'rosebud 'withdraw) 10) "Incorrect password")
(eq? (make-joint acc `wrong `rosebud) "Incorrect password")
(make-joint acc `secret-password `rosebud)
(eq? ((acc 'rosebud 'withdraw) 10) 80)

; SICP 3.8 - Define reset-f!
; This is a function that defines the function f that the exercise
; asks for.

(define f #f)

(define (reset-f!)
  (set! f (lambda (x) (begin (set! f (lambda (x) 0))
                             x))))

; SICP 3.10 - Answer in the comment block.
#|
You have two options:
1) Draw the environment diagram here using ASCII art
2) Submit a .jpg or .pdf file containing your environment diagram (for
example, you could scan a paper drawing), in which case you should
write the filename here.

Environment diagram here

Q. How do the environment structures differ for the two versions?
A. In the *let* case, an additional environment is created with the binding.
In the parameter case, no such mapping is created.

Q. Show that the two versions of make-withdraw create objects with the
same behavior.
A. 

|#

; SICP 3.11 - Answer in the comment block.
#|
Same options as in 3.10

Environment diagram here

Q. Where is the local state for acc kept?
A. In the closure defined by the dispatch function.

Q. How are the local states for the two accounts kept distinct?
A. Two different closures are created.

Q. Which parts of the environment structure are shared?
A. 

|#
