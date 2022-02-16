;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex151) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An N is one of:
; – 0
; – (add1 N)
; interpretation: represents the counting numbers

; N Number -> Number
; computes (* n x) without using *

(check-expect (multiply 4 3.5) (* 4 3.5))
(check-within (multiply 2 pi) (* 2 pi) 0.001)
(check-expect (multiply 78 0.25) (* 78 0.25))

(define (multiply n x)
  (cond
    [(zero? n) 0]
    [(positive? n) (+ x (multiply (sub1 n) x))]))

(multiply 3 5)