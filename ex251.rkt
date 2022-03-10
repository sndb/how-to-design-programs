;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex251) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] -> Number
; computes the sum of the numbers on l
(define (sum l)
  (cond
    [(empty? l) 0]
    [else
     (+ (first l)
        (sum (rest l)))]))

; [List-of Number] -> Number
; computes the product of the numbers on l
(define (product l)
  (cond
    [(empty? l) 1]
    [else
     (* (first l)
        (product (rest l)))]))

; [Number Number -> Number] Number [List-of Number] -> Number
; computes f of the numbers on l using b as the base case

(check-expect (fold1 + 0 '()) (sum '()))
(check-expect (fold1 + 0 '(5)) (sum '(5)))
(check-expect (fold1 + 0 '(1 2 3)) (sum '(1 2 3)))

(check-expect (fold1 * 1 '()) (product '()))
(check-expect (fold1 * 1 '(5)) (product '(5)))
(check-expect (fold1 * 1 '(1 2 3)) (product '(1 2 3)))

(define (fold1 f b l)
  (cond
    [(empty? l) b]
    [else
     (f (first l)
        (fold1 f b (rest l)))]))