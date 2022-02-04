;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex58) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Price falls into one of three intervals:
; — [0, 1000)
; — [1000, 10000)
; — [10000, ∞)
; interpretation the price of an item

(define LOW-PRICE 1000)
(define LUXURY-PRICE 10000)

; Price -> Number
; computes the amount of tax charged for p
(check-expect (sales-tax 0) 0)
(check-expect (sales-tax 537) 0)
(check-expect (sales-tax LOW-PRICE) (* 0.05 LOW-PRICE))
(check-expect (sales-tax 1282) (* 0.05 1282))
(check-expect (sales-tax LUXURY-PRICE) (* 0.08 LUXURY-PRICE))
(check-expect (sales-tax 12017) (* 0.08 12017))
(define (sales-tax p)
  (cond
    [(and (<= 0 p) (< p LOW-PRICE)) 0]
    [(and (<= LOW-PRICE p ) (< p LUXURY-PRICE)) (* 0.05 p)]
    [(>= p LUXURY-PRICE) (* 0.08 p)]))