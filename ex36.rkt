;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex36) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; Image -> Number
; counts the number of pixels in img
; given: (circle 5 "solid" "red"), expected: 100
; given: (rectangle 4 5 "solid" "black"), expected: 20
(define (image-area img)
  (* (image-height img) (image-width img)))