;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex53) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 300)
(define HEIGHT 300)

(define ROCKET (rectangle 5 30 "solid" "red"))
(define SCENE (empty-scene WIDTH HEIGHT))

(define CENTER (/ (image-height ROCKET) 2))
(define YPOS (/ WIDTH 2))


; An LR (short for launching rocket) is one of:
; – "resting"
; – NonnegativeNumber
; interpretation "resting" represents a grounded rocket
; a number denotes the height of a rocket in flight

; grounded rocket, resting
(place-image ROCKET YPOS (- HEIGHT CENTER) SCENE)

; a rocket in flight, height: 0
(place-image ROCKET YPOS (- 0 CENTER) SCENE)

; a rocket in flight, height: 150
(place-image ROCKET YPOS (- 150 CENTER) SCENE)