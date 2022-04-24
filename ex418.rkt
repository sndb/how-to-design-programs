;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex418) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number N -> Number
; raises x to the power of e

(check-expect (my-expt 2 0) 1)
(check-expect (my-expt 2 1) 2)
(check-expect (my-expt 2 2) 4)
(check-expect (my-expt 2 3) 8)
(check-expect (my-expt 10 6) 1000000)

(define (my-expt x e)
  (cond
    [(zero? e) 1]
    [else
     (* x (my-expt x (sub1 e)))]))

(define inex (+ 1 #i1e-12))
(define exac (+ 1 1e-12))

(my-expt inex 30)
(my-expt exac 30)