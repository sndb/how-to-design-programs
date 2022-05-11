;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex461) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ε 0.1)

; [Number -> Number] Number Number -> Number
; integrates a function f between the boundaries a and b using a
; divide-and-conquer strategy
; assume (< a b) holds

(check-within (integrate-adaptive (lambda (x) 20) 12 22) 200 ε)
(check-within (integrate-adaptive (lambda (x) (* 2 x)) 0 10) 100 ε)
(check-within (integrate-adaptive (lambda (x) (* 3 (sqr x))) 0 10)
              1000
              ε)

(define (integrate-adaptive f a b)
  (local ((define mid (/ (+ a b) 2))
          (define area-L (trapezoid-area f a mid))
          (define area-R (trapezoid-area f mid b))
          (define area (trapezoid-area f a b)))
    (cond
      [(< (abs (- area-L area-R)) (* ε (- b a)))
       area]
      [else
       (+ (integrate-adaptive f a mid) (integrate-adaptive f mid b))])))

; [Number -> Number] Number Number -> Number
(define (trapezoid-area f L R)
  (* 1/2 (- R L) (+ (f L) (f R))))