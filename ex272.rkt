;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex272) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; [List-of Any] [List-of Any] -> [List-of Any]
; concatenates l1 and l2

(check-expect (append-from-fold (list 1 2 3) (list 4 5 6 7 8))
              (list 1 2 3 4 5 6 7 8))

(define (append-from-fold l1 l2)
  (foldr cons l2 l1))

; [List-of Number] -> Number
; computes the sum of l

(check-expect (sum '(1 2 3 4)) 10)

(define (sum l)
  (foldl + 0 l))

; [List-of Number] -> Number
; computes the product of l

(check-expect (product '(1 2 3 4)) 24)

(define (product l)
  (foldl * 1 l))

(define SQUARE (square 10 "solid" "red"))
(define CIRCLE (circle 5 "solid" "green"))

; [List-of Image] -> Image
; horizontally composes a list of Images

(check-expect (compose-h `(,SQUARE ,SQUARE ,CIRCLE))
              (beside SQUARE SQUARE CIRCLE))

(define (compose-h l)
  (foldr beside empty-image l))

; [List-of Image] -> Image
; vertically composes a list of Images

(check-expect (compose-v `(,SQUARE ,SQUARE ,CIRCLE))
              (above SQUARE SQUARE CIRCLE))

(define (compose-v l)
  (foldr above empty-image l))