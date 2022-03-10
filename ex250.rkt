;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex250) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [Number -> Number] Number -> [List-of Number]
; tabulates f between n and 0 (incl.) in a list
(define (tabulate f n)
  (cond
    [(zero? n) `(,(f n))]
    [else (cons (f n) (tabulate f (sub1 n)))]))

; Number -> [List-of Number]
(check-expect (tab-sqr 3) '(9 4 1 0))
(define (tab-sqr n)
  (tabulate sqr n))

; Number -> [List-of Number]
(check-within (tab-tan 3)
              `(,(tan 3) ,(tan 2) ,(tan 1) ,(tan 0))
              0.001)
(define (tab-tan n)
  (tabulate tan n))