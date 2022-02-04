;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex51) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define TICK-RATE 1)
(define CIRCLE-RADIUS 10)
(define RED (circle CIRCLE-RADIUS "solid" "red"))
(define GREEN (circle CIRCLE-RADIUS "solid" "green"))
(define YELLOW (circle CIRCLE-RADIUS "solid" "yellow"))

; A TrafficLight is one of the following Strings:
; - "red"
; - "green"
; - "yellow"
; the three strings represent the three possible
; states that a traffic light may assume

; TrafficLight -> TrafficLight
; yields the next state given current state s
(check-expect (traffic-light-next "red") "green")
(check-expect (traffic-light-next "green") "yellow")
(check-expect (traffic-light-next "yellow") "red")
(define (traffic-light-next s)
  (cond
    [(string=? "red" s) "green"]
    [(string=? "green" s) "yellow"]
    [(string=? "yellow" s) "red"]))

; TrafficLight -> Image
; renders the current state s
(check-expect (render "red") RED)
(check-expect (render "green") GREEN)
(check-expect (render "yellow") YELLOW)
(define (render s)
  (cond
    [(string=? "red" s) RED]
    [(string=? "green" s) GREEN]
    [(string=? "yellow" s) YELLOW]))

; Number -> TrafficLight
; simulates a traffic light for a given duration
(define (simulate duration)
  (big-bang "red"
    (on-tick traffic-light-next TICK-RATE (sub1 (/ duration TICK-RATE)))
    (to-draw render)))