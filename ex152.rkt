;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex152) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; An N is one of:
; – 0
; – (add1 N)
; interpretation: represents the counting numbers

; N Image -> Image
; produces a vertical arrangement of n copies of img

(check-expect
 (col 3 (square 20 "solid" "red"))
 (above (square 20 "solid" "red")
        (square 20 "solid" "red")
        (square 20 "solid" "red")))

(define (col n img)
  (cond
    [(zero? n) empty-image]
    [(positive? n) (above img (col (sub1 n) img))]))

; N Image -> Image
; produces a horizontal arrangement of n copies of img

(check-expect
 (row 3 (square 20 "solid" "red"))
 (beside (square 20 "solid" "red")
         (square 20 "solid" "red")
         (square 20 "solid" "red")))

(define (row n img)
  (cond
    [(zero? n) empty-image]
    [(positive? n) (beside img (row (sub1 n) img))]))