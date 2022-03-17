;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex299) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Set is a function:
;   [Any -> Boolean]
; interpretation: if s is a set and x an element, (s x)
; produces #true if x is in s, #false otherwise

; Set
(define (all-odd x)
  (odd? x))

; Set
(define (all-even x)
  (even? x))

; Set
(define (divisible-by-10 x)
  (zero? (modulo x 10)))

; Set Any -> Boolean
(define (inside? s x)
  (s x))

; Set Any -> Set
; adds x to s

(check-expect (inside? (add-element divisible-by-10 11) 11)
              #true)
(check-expect (inside? (add-element divisible-by-10 11) 12)
              #false)
(check-expect (inside? (add-element all-even 3) 3)
              #true)
(check-expect (inside? (add-element all-even 3) 4)
              #true)
(check-expect (inside? (add-element all-even 3) 5)
              #false)

(define (add-element s x)
  (lambda (y)
    (or (equal? y x)
        (s y))))

; Set Set -> Set
; combines the elements of two sets

(check-expect (inside? (union all-even all-odd) 1)
              #true)
(check-expect (inside? (union all-even all-odd) 2)
              #true)
(check-expect (inside? (union all-even divisible-by-10) 4)
              #true)
(check-expect (inside? (union all-even divisible-by-10) 10)
              #true)
(check-expect (inside? (union all-even divisible-by-10) 5)
              #false)

(define (union s1 s2)
  (lambda (y)
    (or (inside? s1 y)
        (inside? s2 y))))

; Set Set -> Set
; collects all elements common to two sets

(check-expect (inside? (intersect all-even all-odd) 1)
              #false)
(check-expect (inside? (intersect all-even all-odd) 2)
              #false)
(check-expect (inside? (intersect all-even divisible-by-10) 4)
              #false)
(check-expect (inside? (intersect all-even divisible-by-10) 10)
              #true)
(check-expect (inside? (intersect all-even divisible-by-10) 5)
              #false)

(define (intersect s1 s2)
  (lambda (y)
    (and (inside? s1 y)
         (inside? s2 y))))