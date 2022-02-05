;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex101) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 200)
(define HEIGHT 200)
(define UFO-WIDTH 24)
(define UFO-HEIGHT 12)
(define TANK-WIDTH 20)
(define TANK-HEIGHT 8)
(define MISSILE-SIZE 8)

(define UFO (overlay (circle (/ UFO-HEIGHT 2) "solid" "green")
                     (rectangle UFO-WIDTH (/ UFO-HEIGHT 3) "solid" "green")))
(define TANK (rectangle TANK-WIDTH TANK-HEIGHT "solid" "darkgreen"))
(define MISSILE (triangle MISSILE-SIZE "solid" "red"))
(define SCENE (empty-scene WIDTH HEIGHT "darkblue"))

(define-struct sigs [ufo tank missile])
; A SIGS.v2 (short for SIGS version 2) is a structure:
;   (make-sigs UFO Tank MissileOrNot)
; interpretation: represents the complete state of a
; space invader game

; A MissileOrNot is one of:
; – #false
; – Posn
; interpretation: #false means the missile is in the tank;
; Posn says the missile is at that location

; MissileOrNot Image -> Image
; adds an image of missile m to scene s
(check-expect (missile-render.v2 #false SCENE) SCENE)
(check-expect (missile-render.v2 (make-posn 32
                                            (- HEIGHT
                                               TANK-HEIGHT
                                               10))
                                 SCENE)
              (place-image MISSILE 32 (- HEIGHT TANK-HEIGHT 10) SCENE))
(define (missile-render.v2 m s)
  s)