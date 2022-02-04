;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex81) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Time is a structure:
;   (make-time Hour Minute Second)
; A Hour is a Number between 0 and 23
; A Minute is a Number between 0 and 59
; A Second is a Number between 0 and 59
; interpretation: point in time since midnight
(define-struct time [hours minutes seconds])

; Time -> Number
; produces the number of seconds that have passed since midnight
(check-expect (time->seconds (make-time 12 30 2)) 45002)
(check-expect (time->seconds (make-time 9 24 53)) 33893)
(define (time->seconds t)
  (+ (* 60 60 (time-hours t))
     (* 60 (time-minutes t))
     (time-seconds t)))