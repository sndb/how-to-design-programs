;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex43) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define SPEED 3)
(define SINE-MULTIPLIER 10)

; An AnimationState is a Number.
; interpretation the number of clock ticks 
; since the animation started

; AnimationState -> Number
; determine the car position according to its speed
; and the given world state
(check-within (position 50) (+ (* 50 SPEED) (* SINE-MULTIPLIER (sin 50))) 0.01)
(check-within (position 32) (+ (* 32 SPEED) (* SINE-MULTIPLIER (sin 32))) 0.01)
(define (position cw)
  (+ (* cw SPEED) (* SINE-MULTIPLIER (sin cw))))

; AnimationState -> Image
; places the car into the BACKGROUND scene,
; according to the given world state
(check-expect (render 50) (place-image CAR (position 50) Y-CAR BACKGROUND))
(check-expect (render 32) (place-image CAR (position 32) Y-CAR BACKGROUND))
(define (render cw)
  (place-image CAR (position cw) Y-CAR BACKGROUND))
 
; AnimationState -> AnimationState 
; increment cw by 1 for every clock tic
(check-expect (tock 20) 21)
(check-expect (tock 78) 79)
(define (tock cw)
  (add1 cw))

; AnimationState -> Boolean
; stops the animation when the car has disappeared on the right side
(check-expect (end? 50) (> (position 50) (+ (/ (image-width CAR) 2) (image-width BACKGROUND))))
(check-expect (end? 300) (> (position 300) (+ (/ (image-width CAR) 2) (image-width BACKGROUND))))
(define (end? cw)
  (> (position cw) (+ (/ (image-width CAR) 2) (image-width BACKGROUND))))

; AnimationState -> AnimationState
; launches the program from some initial state 
(define (main ws)
  (big-bang ws
    [on-tick tock]
    [to-draw render]
    [stop-when end?]))