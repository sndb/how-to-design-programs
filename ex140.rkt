;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex140) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-booleans is one of:
; — '()
; — (cons Boolean List-of-booleans)

(define ex1 '())
(define ex2 (cons #true '()))
(define ex3 (cons #false '()))
(define ex4 (cons #true (cons #false '())))
(define ex5 (cons #false (cons #false '())))
(define ex6 (cons #true (cons #true '())))
(define ex7 (cons #false (cons #false (cons #true '()))))

; List-of-booleans -> Boolean
; determines whether all items of l are #true

(check-expect (all-true ex1) #true)
(check-expect (all-true ex2) #true)
(check-expect (all-true ex3) #false)
(check-expect (all-true ex4) #false)
(check-expect (all-true ex5) #false)
(check-expect (all-true ex6) #true)
(check-expect (all-true ex7) #false)

(define (all-true l)
  (cond
    [(empty? l) #true]
    [else
     (and (first l)
          (all-true (rest l)))]))

; List-of-booleans -> Boolean
; determines whether at least one item of l is #true

(check-expect (one-true ex1) #false)
(check-expect (one-true ex2) #true)
(check-expect (one-true ex3) #false)
(check-expect (one-true ex4) #true)
(check-expect (one-true ex5) #false)
(check-expect (one-true ex6) #true)
(check-expect (one-true ex7) #true)

(define (one-true l)
  (cond
    [(empty? l) #false]
    [else
     (or (first l)
         (one-true (rest l)))]))