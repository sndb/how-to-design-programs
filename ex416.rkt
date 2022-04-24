;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex416) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number N -> N
; determines the integer n such that (expt x n) is a number
; while (expt x (+ n 1)) is +inf.0
(define (largest-exponent x i)
  (cond
    [(= +inf.0 (expt x (add1 i))) i]
    [else (largest-exponent x (add1 i))]))

; Number N -> N
; determines the integer n such that (expt x n) is a number
; while (expt x (- n 1)) is #i0.0
(define (smallest-exponent x i)
  (cond
    [(= #i0.0 (expt x (sub1 i))) i]
    [else (smallest-exponent x (sub1 i))]))

; Number N Number [N -> N] -> N
; determines the integer n such that (expt x n) is a number
; while (expt x (f n)) is ref

(check-expect (find-exponent #i10. 0 +inf.0 add1)
              (largest-exponent #i10. 0))
(check-expect (find-exponent #i10. 0 #i0.0 sub1)
              (smallest-exponent #i10. 0))

(define (find-exponent x i r f)
  (cond
    [(= r (expt x (f i))) i]
    [else (find-exponent x (f i) r f)]))