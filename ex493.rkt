;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex493) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of X] -> [List-of X]
; constructs the reverse of alox
 
(check-expect (invert '(a b c)) '(c b a))
 
(define (invert alox)
  (cond
    [(empty? alox) '()]
    [else
     (add-as-last (first alox) (invert (rest alox)))]))
 
; X [List-of X] -> [List-of X]
; adds an-x to the end of alox
 
(check-expect (add-as-last 'a '(c b)) '(c b a))
 
(define (add-as-last an-x alox)
  (cond
    [(empty? alox) (list an-x)]
    [else
     (cons (first alox) (add-as-last an-x (rest alox)))]))

; ‘add-as-last’ goes through the whole list once to insert an element
; to its end. ‘invert’ applies ‘add-as-last’ N times for the list of
; length N. The average length of input to ‘add-as-last’ is 1/2 * N.
; Hence, ‘invert’ performs 1/2 * N^2 recursive steps for the list of
; length N.

; A function f is a member of O(g) if there exist numbers c and
; bigEnough such that:
; for all n >= bigEnough it is true that f(n) <= c * g(n).

(define c 1)
(define bigEnough 1)
(define (f n) (* 1/2 n n))
(define (g n) (* n n))

(<= (f bigEnough) (* c (g bigEnough)))

; ‘invert’ is indeed belongs to O(n2).