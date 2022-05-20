;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex528) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define COLOR 'red)
(define THRESHOLD 10)

; Image Posn Posn Posn -> Image
; adds the bezier curve to the image using a and c as the end points
; and b as the perspective point
(define (add-bezier image a c b)
  (cond
    [(< (distance a c) THRESHOLD)
     (scene+line
      image (posn-x a) (posn-y a) (posn-x c) (posn-y c) COLOR)]
    [else
     (local ((define a-b (mid-point a b))
             (define b-c (mid-point b c))
             (define a-b-c (mid-point a-b b-c))
             (define i0 (add-bezier image a a-b-c a-b))
             (define i1 (add-bezier i0 c a-b-c b-c)))
       i1)]))

; Posn Posn -> Posn 
; determines the midpoint between a and b

(check-expect (mid-point (make-posn 1 1) (make-posn 0 0))
              (make-posn 0.5 0.5))
(check-expect (mid-point (make-posn 0 0) (make-posn 2 0))
              (make-posn 1 0))
(check-expect (mid-point (make-posn 0 0) (make-posn 0 2))
              (make-posn 0 1))

(define (mid-point a b)
  (make-posn (* 1/2 (+ (posn-x a) (posn-x b)))
             (* 1/2 (+ (posn-y a) (posn-y b)))))

; Posn Posn -> Number
; determines the distance between a and b

(check-expect (distance (make-posn 1 0) (make-posn 0 0)) 1)
(check-within (distance (make-posn 1 0) (make-posn 0 1)) 1.41 0.1)

(define (distance a b)
  (sqrt (+ (sqr (- (posn-x a) (posn-x b)))
           (sqr (- (posn-y a) (posn-y b))))))

(add-bezier (empty-scene 100 100)
            (make-posn 10 10)
            (make-posn 90 50)
            (make-posn 10 90))