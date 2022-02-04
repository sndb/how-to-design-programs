;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex39) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 5))

(define WHEEL (circle WHEEL-RADIUS "solid" "black"))
(define SPACE (rectangle WHEEL-DISTANCE WHEEL-RADIUS "solid" "white"))
(define BOTH-WHEELS (beside WHEEL SPACE WHEEL))

(define LOWER-BODY (rectangle (* WHEEL-RADIUS 10) (* WHEEL-RADIUS 3) "solid" "darkolivegreen"))
(define UPPER-BODY (rectangle (* WHEEL-RADIUS 7) (* WHEEL-RADIUS 2) "solid" "darkolivegreen"))
(define BODY (above/align "left" UPPER-BODY LOWER-BODY))

(define CAR (underlay/xy BODY (* WHEEL-RADIUS 0.6) (* WHEEL-RADIUS 4) BOTH-WHEELS))