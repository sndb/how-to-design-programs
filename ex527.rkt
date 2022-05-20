;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex527) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define COLOR 'red)
(define THRESHOLD 10)
(define L-LENGTH-DELTA 2/3)
(define L-ANGLE-DELTA 0.15)
(define R-LENGTH-DELTA 4/5)
(define R-ANGLE-DELTA -0.2)

; Image Number Number Number Number -> Image
; adds a fractal Savannah tree to the given image
; generative: adds the line ((x, y), end-point) to the image, then
; recurs at the two intermediate points
; terminates only if
; (and (> THRESHOLD 0) (< L-LENGTH-DELTA 1) (< R-LENGTH-DELTA 1))
(define (add-savannah scene x y length angle)
  (cond
    [(< length THRESHOLD) scene]
    [else
     (local ((define end-point
               (circle-pt (make-posn x y) length angle))
             (define l-point
               (circle-pt (make-posn x y) (* 1/3 length) angle))
             (define r-point
               (circle-pt (make-posn x y) (* 2/3 length) angle))
             (define i0 (scene+line scene
                                    x y
                                    (posn-x end-point)
                                    (posn-y end-point)
                                    COLOR))
             (define i1 (add-savannah i0
                                      (posn-x l-point)
                                      (posn-y l-point)
                                      (* length L-LENGTH-DELTA)
                                      (+ angle L-ANGLE-DELTA)))
             (define i2 (add-savannah i1
                                      (posn-x r-point)
                                      (posn-y r-point)
                                      (* length R-LENGTH-DELTA)
                                      (+ angle R-ANGLE-DELTA))))
       i2)]))

; Posn Number Number -> Posn
; determines the point on the circle with center 
; and radius whose angle is 
 
(check-within (circle-pt (make-posn 200 200) 200 (* 1/2 pi))
              (make-posn 200 0) 0.001)
(check-within (circle-pt (make-posn 200 200) 200 pi)
              (make-posn 0 200) 0.001)
(check-within (circle-pt (make-posn 200 200) 200 (* 3/2 pi))
              (make-posn 200 400) 0.001)
(check-within (circle-pt (make-posn 200 200) 200 (* 2 pi))
              (make-posn 400 200) 0.001)
 
(define (circle-pt center radius angle)
  (make-posn (+ (posn-x center) (* radius (cos angle)))
             (- (posn-y center) (* radius (sin angle)))))

(add-savannah (empty-scene 400 400)
              200 380
              120
              (/ pi 2))