;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex163) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An F is a Number in the interval [-459,∞)
; interpretation: represents a temperature in Fahrenheit

; A C is a Number in the interval [-273,∞)
; interpretation: represents a temperature in Celsius

; A List-of-f is one of:
; — '()
; — (cons F List-of-f)
; interpretation: a list of measurements in Fahrenheit

; A List-of-c is one of:
; — '()
; — (cons C List-of-c)
; interpretation: a list of measurements in Celsius

; List-of-f -> List-of-c
; converts a list of measurements in Fahrenheit to
; a list of Celsius measurements

(check-expect (convertFC '()) '())
(check-expect (convertFC (cons -40 '()))
              (cons (F->C -40) '()))
(check-expect (convertFC (cons -40 (cons 0 (cons 212 '()))))
              (cons (F->C -40) (cons (F->C 0) (cons (F->C 212) '()))))

(define (convertFC lof)
  (cond
    [(empty? lof) '()]
    [else
     (cons (F->C (first lof)) (convertFC (rest lof)))]))

; F -> C
; convert a temperature in Fahrenheit to Celsius

(check-expect (F->C -40) -40)
(check-expect (F->C 32) 0)
(check-expect (F->C 212) 100)

(define (F->C f)
  (/ (- f 32) 1.8))