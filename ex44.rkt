;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex44) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 5))
(define LOWER-BODY-WIDTH (* WHEEL-RADIUS 10))
(define LOWER-BODY-HEIGHT (* WHEEL-RADIUS 3))
(define UPPER-BODY-WIDTH (* WHEEL-RADIUS 7))
(define UPPER-BODY-HEIGHT (* WHEEL-RADIUS 2))
(define WHEELS-X (* WHEEL-RADIUS 0.6))
(define WHEELS-Y (* WHEEL-RADIUS 4))

(define WHEEL (circle WHEEL-RADIUS "solid" "black"))
(define SPACE (rectangle WHEEL-DISTANCE WHEEL-RADIUS "solid" "white"))
(define BOTH-WHEELS (beside WHEEL SPACE WHEEL))
(define LOWER-BODY (rectangle LOWER-BODY-WIDTH LOWER-BODY-HEIGHT "solid" "darkolivegreen"))
(define UPPER-BODY (rectangle UPPER-BODY-WIDTH UPPER-BODY-HEIGHT "solid" "darkolivegreen"))
(define BODY (above/align "left" UPPER-BODY LOWER-BODY))
(define CAR (underlay/xy BODY WHEELS-X WHEELS-Y BOTH-WHEELS))

(define WIDTH-OF-WORLD 200)
(define Y-CAR 20)
(define TREE
  (underlay/xy (circle 10 "solid" "green")
               9 15
               (rectangle 2 20 "solid" "brown")))
(define SCENE (empty-scene WIDTH-OF-WORLD (+ Y-CAR (/ (image-height CAR) 2))))
(define BACKGROUND (place-image TREE 150 (- (image-height SCENE) (/ (image-height TREE) 2)) SCENE))

; A WorldState is a Number.
; interpretation: the number of pixels between the left border
; of the scene and the right-most edge of the car

; WorldState -> Image
; places the car into the BACKGROUND scene,
; according to the given world state
(check-expect (render 50) (place-image CAR 50 Y-CAR BACKGROUND))
(check-expect (render 127) (place-image CAR 127 Y-CAR BACKGROUND))
(define (render cw)
  (place-image CAR cw Y-CAR BACKGROUND))
 
; WorldState -> WorldState 
; moves the car by 3 pixels for every clock tick
(check-expect (tock 20) 23)
(check-expect (tock 78) 81)
(define (tock cw)
  (+ cw 3))

; WorldState Number Number String -> WorldState
; places the car at x-mouse
; if the given me is "button-down"
(check-expect (hyper 21 10 20 "enter") 21)
(check-expect (hyper 42 10 20 "button-down") 10)
(check-expect (hyper 42 10 20 "move") 42)
(define (hyper x-position-of-car x-mouse y-mouse me)
  (cond
    [(string=? "button-down" me) x-mouse]
    [else x-position-of-car]))

; WorldState -> Boolean
; stops the animation when the car has disappeared on the right side
(check-expect (end? 50) #false)
(check-expect (end? 300) #true)
(define (end? cw)
  (> cw (+ (/ (image-width CAR) 2) (image-width BACKGROUND))))

; WorldState -> WorldState
; launches the program from some initial state 
(define (main ws)
  (big-bang ws
    [on-tick tock]
    [on-mouse hyper]
    [to-draw render]
    [stop-when end?]))