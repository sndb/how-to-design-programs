;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex77) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Time is a structure:
;   (make-time Hour Minute Second)
; A Hour is a Number between 0 and 23
; A Minute is a Number between 0 and 59
; A Second is a Number between 0 and 59
; interpretation: point in time since midnight
(define-struct time [hours minutes seconds])