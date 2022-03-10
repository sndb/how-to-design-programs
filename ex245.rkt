;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex245) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (f x) x)
(define (g x) (if (> x 0) x (- x)))

(check-expect (function=at-1.2-3-and-5.775? f f) #true)
(check-expect (function=at-1.2-3-and-5.775? f g) #false)

(define (function=at-1.2-3-and-5.775? f1 f2)
  (and (= (f1 1.2) (f2 1.2))
       (= (f1 3) (f2 3))
       (= (f1 -5.775) (f2 -5.775))))