;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex526) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define CENTER (make-posn 200 200))
(define RADIUS 200) ; the radius in pixels 
 
; Number -> Posn
; determines the point on the circle with CENTER 
; and RADIUS whose angle is 
 
(check-within (circle-pt 90/360) (make-posn 200 0) 0.001)
(check-within (circle-pt 180/360) (make-posn 0 200) 0.001)
(check-within (circle-pt 270/360) (make-posn 200 400) 0.001)
(check-within (circle-pt 360/360) (make-posn 400 200) 0.001)
 
(define (circle-pt factor)
  (local ((define angle (* 2 pi factor)))
    (make-posn (+ (posn-x CENTER) (* RADIUS (cos angle)))
               (- (posn-y CENTER) (* RADIUS (sin angle))))))