;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex398) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A LinearCombination is a [List-of Number].
; interpretation: represents a list of coefficients of some
; linear combination

; LinearCombination [List-of Number] -> Number
; produces the value of the combination for variable values lv
; assume: input lists are of same length

(check-expect (value '(5) '(10)) 50)
(check-expect (value '(5 17) '(10 1)) 67)
(check-expect (value '(5 17 3) '(10 1 2)) 73)

(define (value lc lv)
  (cond
    [(empty? lc) 0]
    [(cons? lc)
     (+ (* (first lc) (first lv))
        (value (rest lc) (rest lv)))]))