;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex105) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define DOT (circle 5 "solid" "red"))
(define SCENE (empty-scene 100 100))

; A Coordinate is one of:
; – a NegativeNumber
; interpretation: on the y axis, distance from top
; – a PositiveNumber
; interpretation: on the x axis, distance from left
; – a Posn
; interpretation: an ordinary Cartesian point

-1
(place-image DOT 0 1 SCENE)

-42
(place-image DOT 0 42 SCENE)

1
(place-image DOT 1 0 SCENE)

64
(place-image DOT 64 0 SCENE)

(make-posn 17 25)
(place-image DOT 17 25 SCENE)

(make-posn 39 34)
(place-image DOT 39 42 SCENE)