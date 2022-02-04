;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex96) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; physical constants
(define WIDTH 200)
(define HEIGHT 200)

(define TANK-SPEED 40)
(define UFO-DESCEND-SPEED 10)
(define UFO-JUMP-SPEED 5)
(define MISSILE-SPEED (* 2 UFO-DESCEND-SPEED))

(define UFO-WIDTH 24)
(define UFO-HEIGHT 12)
(define TANK-WIDTH 20)
(define TANK-HEIGHT 8)
(define MISSILE-SIZE 8)
(define SURFACE-HEIGHT (/ HEIGHT 6))

; graphical constants
(define UFO (overlay (circle (/ UFO-HEIGHT 2) "solid" "green")
                     (rectangle UFO-WIDTH (/ UFO-HEIGHT 3) "solid" "green")))
(define TANK (rectangle TANK-WIDTH TANK-HEIGHT "solid" "darkgreen"))
(define MISSILE (triangle MISSILE-SIZE "solid" "red"))
(define SCENE (empty-scene WIDTH HEIGHT "darkblue"))
(define SURFACE (empty-scene WIDTH SURFACE-HEIGHT "lightbrown"))
(define PLANET (above SCENE SURFACE))

; data definitions

; A UFO is a Posn.
; interpretation: (make-posn x y) is the UFO's location
; (using the top-down, left-to-right convention)

(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number).
; interpretation: (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick

; A Missile is a Posn.
; interpretation: (make-posn x y) is the missile's place

(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])
; A SIGS is one of:
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; interpretation: represents the complete state of a 
; space invader game

(make-aim (make-posn 20 10) (make-tank 28 -3))
(place-image UFO
             20 10
             (place-image TANK
                          28 HEIGHT
                          PLANET))

(make-fired (make-posn 20 10)
            (make-tank 28 -3)
            (make-posn 28 (- HEIGHT TANK-HEIGHT)))
(place-image MISSILE
             28 (- HEIGHT TANK-HEIGHT)
             (place-image UFO
                          20 10
                          (place-image TANK
                                       28 HEIGHT
                                       PLANET)))

(make-fired (make-posn 20 100)
            (make-tank 100 3)
            (make-posn 22 103))
(place-image MISSILE
             22 103
             (place-image UFO
                          20 100
                          (place-image TANK
                                       100 HEIGHT
                                       PLANET)))