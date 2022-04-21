;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex393) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Son.L is one of: 
; – empty 
; – (cons Number Son.L)

; A Son.R is one of: 
; – empty 
; – (cons Number Son.R)

; Son is used when it 
; applies to Son.L and Son.R
  
; Constraint If s is a Son.R, 
; no number occurs twice in s

; Son Son -> Son
; produces the set that contains the elements of both s1 and s2

(check-expect (union '(1 2 3) '(3 4 5)) '(1 2 3 4 5))
(check-expect (union '(1 2 3) '()) '(1 2 3))
(check-expect (union '() '(3 4 5)) '(3 4 5))
(check-expect (union '() '()) '())

(define (union s1 s2)
  (cond
    [(empty? s1) s2]
    [(cons? s1)
     (if (member? (first s1) s2)
         (union (rest s1) s2)
         (cons (first s1) (union (rest s1) s2)))]))

; Son Son -> Son
; produces the set that contains the elements that occur in
; both s1 and s2

(check-expect (intersect '(1 2 3) '(3 4 5)) '(3))
(check-expect (intersect '(1 2 3) '()) '())
(check-expect (intersect '() '(3 4 5)) '())
(check-expect (intersect '() '()) '())

(define (intersect s1 s2)
  (cond
    [(empty? s1) '()]
    [(cons? s1)
     (if (member? (first s1) s2)
         (cons (first s1) (intersect (rest s1) s2))
         (intersect (rest s1) s2))]))