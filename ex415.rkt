;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex415) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number -> N
; determines the integer n such that (expt x n) is a number
; while (expt x (+ n 1)) is approximated with +inf.0
(define (largest-exponent x i)
  (cond
    [(= +inf.0 (expt x (add1 i))) i]
    [else (largest-exponent x (add1 i))]))