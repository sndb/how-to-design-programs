;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex458) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ε 0.1)

; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; using the Kelper's rule
; assume (< a b) holds

(check-within (integrate-kepler (lambda (x) 20) 12 22) 200 ε)
(check-within (integrate-kepler (lambda (x) (* 2 x)) 0 10) 100 ε)
;(check-within (integrate-kepler (lambda (x) (* 3 (sqr x))) 0 10)
;              1000
;              ε)
; fails by 125

(define (integrate-kepler f a b)
  (local (; Number
          (define mid (/ (+ a b) 2))
          ; Number Number -> Number
          (define (area L R) (* 1/2 (- R L) (+ (f L) (f R)))))
    (+ (area a mid) (area mid b))))