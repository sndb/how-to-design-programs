;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex501) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; N -> Number 
; adds n to pi without using +

(check-within (add-to-pi 2) (+ 2 pi) 0.001)

(define (add-to-pi n)
  (cond
    [(zero? n) pi]
    [else (add1 (add-to-pi (sub1 n)))]))

; N -> Number 
; adds n to pi without using +, accumulator version

(check-within (add-to-pi.v2 2) (+ 2 pi) 0.001)

(define (add-to-pi.v2 n)
  (local (; N Number -> Number
          ; accumulator a is pi + n0 - n
          (define (add-to-pi/a n a)
            (cond
              [(zero? n) a]
              [else
               (add-to-pi/a (sub1 n) (add1 a))])))
    (add-to-pi/a n pi)))