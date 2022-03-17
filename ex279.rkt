;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex279) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; legal
(lambda (x y) (x y y))

; illegal, functions need arguments
(lambda () 10)

; legal
(lambda (x) x)

; legal
(lambda (x y) x)

; illegal, x must be inside of parentheses
(lambda x 10)