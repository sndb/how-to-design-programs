;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex104) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct vehicle [passengers license fuel-consumption])
; A Vehicle is a structure:
;   (make-struct Number String Number)
; interpretation: describes the number of passengers the vehicle can carry, its
; license number, and its fuel consumption (miles per gallon)

; A template for functions that consume vehicles:
; (... (vehicle-passengers v) ... (vehicle-license v) ... (vehicle-fuel-consumption v) ...)