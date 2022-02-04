;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex47) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define MAX-HAPPINESS 100)
(define MIN-HAPPINESS 0)
(define FRAME-WIDTH 8)
(define FRAME (empty-scene FRAME-WIDTH MAX-HAPPINESS))

; A WorldState is a Number
; the current happiness score

; WorldState -> Image
; display the happiness gauge
(check-expect (render 50) (place-image (rectangle (- FRAME-WIDTH 2) 50 "solid" "red")
                                       (/ FRAME-WIDTH 2)
                                       (- (- MAX-HAPPINESS (/ 50 2)) 1)
                                       FRAME))
(define (render cw)
  (place-image (rectangle (- FRAME-WIDTH 2) cw "solid" "red")
               (/ FRAME-WIDTH 2)
               (- (- MAX-HAPPINESS (/ cw 2)) 1)
               FRAME))

; WorldState -> WorldState
; decrease the level of happiness by 0.1
; it never falls below 0, the minimum happiness score
(check-expect (tock 5) 4.9)
(check-expect (tock -5) 0)
(check-expect (tock 0) 0)
(define (tock cw)
  (clamp-happiness (- cw 0.1)))

; WorldState String -> WorldState
; if the down arrow key is pressed, decrease happiness by 1/5
; if the up arrow key is pressed, increase happiness by 1/3
(check-expect (ke-h 50 "down") (- 50 (* MAX-HAPPINESS 1/5)))
(check-expect (ke-h 50 "up") (+ 50 (* MAX-HAPPINESS 1/3)))
(check-expect (ke-h 50 "left") 50)
(define (ke-h cw ke)
  (clamp-happiness
   (cond
     [(string=? ke "down") (- cw (* MAX-HAPPINESS 1/5))]
     [(string=? ke "up") (+ cw (* MAX-HAPPINESS 1/3))]
     [else cw])))

; WorldState -> WorldState
; keep happiness between its bound
(check-expect (clamp-happiness 150) 100)
(check-expect (clamp-happiness -20) 0)
(check-expect (clamp-happiness 50) 50)
(define (clamp-happiness cw)
  (cond
    [(> cw MAX-HAPPINESS) MAX-HAPPINESS]
    [(< cw MIN-HAPPINESS) MIN-HAPPINESS]
    [else cw]))

; WorldState -> WorldState
(define (gauge-prog cw)
  (big-bang cw
    [to-draw render]
    [on-tick tock]
    [on-key ke-h]))