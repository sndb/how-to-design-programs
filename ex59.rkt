;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex59) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 90)
(define HEIGHT 30)
(define BULB-RADIUS 8)
(define BULB-DIAMETER (* 2 BULB-RADIUS))
(define SPACE-BETWEEN (/ (- WIDTH (* 3 (* 2 BULB-RADIUS))) 4))

(define SPACER (rectangle SPACE-BETWEEN 1 "solid" "transparent"))
(define MTSCN (empty-scene WIDTH HEIGHT))

; A TrafficLight is one of the following Strings:
; – "red"
; – "green"
; – "yellow"
; interpretation the three strings represent the three 
; possible states that a traffic light may assume 

; TrafficLight -> TrafficLight
; yields the next state, given current state cs
(check-expect (tl-next "red") "green")
(check-expect (tl-next "green") "yellow")
(check-expect (tl-next "yellow") "red")
(define (tl-next cs)
  (cond
    [(string=? "red" cs) "green"]
    [(string=? "green" cs) "yellow"]
    [(string=? "yellow" cs) "red"]))
 
; TrafficLight -> Image
; renders the current state cs as an image
(check-expect (tl-render "red")
              (place-image (bulb "red" #true)
                           (- (/ WIDTH 2) BULB-DIAMETER SPACE-BETWEEN)
                           (/ HEIGHT 2)
                           BG))
(check-expect (tl-render "green")
              (place-image (bulb "green" #true)
                           (+ (/ WIDTH 2) BULB-DIAMETER SPACE-BETWEEN)
                           (/ HEIGHT 2)
                           BG))
(check-expect (tl-render "yellow")
              (place-image (bulb "yellow" #true)
                           (/ WIDTH 2) (/ HEIGHT 2)
                           BG))
(define (tl-render cs)
  (place-image (bulb cs #true)
               (+ (cond
                    [(string=? "red" cs) (- 0 BULB-DIAMETER SPACE-BETWEEN)]
                    [(string=? "green" cs) (+ BULB-DIAMETER SPACE-BETWEEN)]
                    [(string=? "yellow" cs) 0])
                  (/ WIDTH 2))
               (/ HEIGHT 2)
               BG))

; TrafficLight Boolean -> Image
; crates the image of a one-color bulb
(check-expect (bulb "red" #true) (circle BULB-RADIUS "solid" "red"))
(check-expect (bulb "green" #true) (circle BULB-RADIUS "solid" "green"))
(check-expect (bulb "yellow" #false) (circle BULB-RADIUS "outline" "yellow"))
(define (bulb cs on)
  (circle BULB-RADIUS (if on "solid" "outline") cs))

(define BG (place-image
            (beside (bulb "red" #false)
                    SPACER
                    (bulb "yellow" #false)
                    SPACER
                    (bulb "green" #false))
            (/ WIDTH 2) (/ HEIGHT 2)
            MTSCN))

; TrafficLight -> TrafficLight
; simulates a clock-based American traffic light
(define (traffic-light-simulation initial-state)
  (big-bang initial-state
    [to-draw tl-render]
    [on-tick tl-next 1]))