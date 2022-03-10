;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex244) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (f x) (x 10))
(define (f x) (x f))
(define (f x y) (x 'a y 'b))

; (1) the first position in an application allow variables
;     and function parameters in ISL
; (2) functions and primitive operations are values, so they
;     can be used as arguments of other functions in ISL