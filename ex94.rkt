;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex94) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; physical constants
(define WIDTH 240)
(define HEIGHT 480)
(define TANK-SPEED 60)
(define UFO-DESCEND-SPEED 20)
(define UFO-JUMP-SPEED 10)
(define MISSILE-SPEED (* 2 UFO-DESCEND-SPEED))
(define UFO-SIZE 40)
(define TANK-SIZE 40)
(define MISSILE-SIZE 10)
(define SURFACE-LEVEL (/ HEIGHT 6))

; graphical constants
(define UFO (overlay (circle (* UFO-SIZE 1/4) "solid" "green")
                     (rectangle UFO-SIZE (* UFO-SIZE 1/4) "solid" "green")))
(define TANK (above (rectangle (* TANK-SIZE 1/10) (* TANK-SIZE 1/5) "solid" "darkgreen")
                    (rectangle (* TANK-SIZE 1/2) (* TANK-SIZE 1/5) "solid" "darkgreen")
                    (rectangle TANK-SIZE (* TANK-SIZE 1/3) "solid" "darkgreen")))
(define MISSILE (triangle MISSILE-SIZE "solid" "red"))
(define SCENE (empty-scene WIDTH HEIGHT "darkblue"))
(define PLANET (overlay/align "center" "bottom"
                              (rectangle WIDTH SURFACE-LEVEL "solid" "lightbrown")
                              SCENE))

; A Game is a structure:
;   (make-game Posn Number Posn)
; interpretation: (make-game u t m) describes the ufo's position u, the tank's
; position t, and the missile's position m
(define-struct game [ufo tank missile])

(place-image MISSILE
             50 (- HEIGHT SURFACE-LEVEL (* HEIGHT 1/3))
             (place-image UFO
                          140 (- HEIGHT SURFACE-LEVEL (/ HEIGHT 2))
                          (place-image TANK
                                       50 (- HEIGHT SURFACE-LEVEL (/ (image-height TANK) 2))
                                       PLANET)))