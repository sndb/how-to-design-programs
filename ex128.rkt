;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex128) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(check-member-of "green" "red" "yellow" "grey")
; "green" is not one of "red", "yellow", or "grey"

(check-within (make-posn #i1.0 #i1.1)
              (make-posn #i0.9 #i1.2)  0.01)
; (make-posn #i1.0 #i1.1) is not within 0.01
; of (make-posn #i0.9 #i1.2)

(check-range #i0.9 #i0.6 #i0.8)
; #i0.9 is not between #i0.6 and #i0.8

(check-random (make-posn (random 3) (random 9))
              (make-posn (random 9) (random 3)))
; (make-posn (random 3) (random 9)) not evaluates to
; (make-posn (random 9) (random 3))

(check-satisfied 4 odd?)
; 4 is not odd