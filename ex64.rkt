;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex64) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; measures the manhattan distance of ap to the origin
(check-expect (manhattan-distance (make-posn 4 3)) 7)
(check-expect (manhattan-distance (make-posn 5 5)) 10)
(check-expect (manhattan-distance (make-posn 7 13)) 20)
(define (manhattan-distance ap)
  (+ (posn-x ap) (posn-y ap)))