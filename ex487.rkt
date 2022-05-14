;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex487) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define bigEnough 10)
(define c 1)

(define (f n)
  (expt 2 n))

(define (g n)
  (* 100 n))

(<= (g bigEnough) (* c (f bigEnough)))

(<= (g 3) (* c (f 3)))
(<= (g 9) (* c (f 9)))
(<= (g 10) (* c (f 10)))
(<= (g 12) (* c (f 12)))

; Q: If the input size is guaranteed to be between 3 and 12, which
; function is better?
; A: f is better on [3, 9], g is better on [10, 12].