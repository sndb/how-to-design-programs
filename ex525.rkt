;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex525) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define THRESHOLD 8)
(define COLOR 'red)

(define MT (empty-scene 400 400))
(define A (make-posn 200  50))
(define B (make-posn  27 350))
(define C (make-posn 373 350))

; Image Posn Posn Posn -> Image 
; generative adds the triangle (a, b, c) to scene0, 
; subdivides it into three triangles by taking the 
; midpoints of its sides; stop if (a, b, c) is too small
; accumulator the function accumulates the triangles of scene0
(define (add-sierpinski scene0 a b c)
  (cond
    [(too-small? a b c) scene0]
    [else
     (local
       ((define scene1 (add-triangle scene0 a b c))
        (define mid-a-b (mid-point a b))
        (define mid-b-c (mid-point b c))
        (define mid-c-a (mid-point c a))
        (define scene2
          (add-sierpinski scene1 a mid-a-b mid-c-a))
        (define scene3
          (add-sierpinski scene2 b mid-b-c mid-a-b)))
       ; —IN—
       (add-sierpinski scene3 c mid-c-a mid-b-c))]))

; Image Posn Posn Posn -> Image 
; adds the black triangle a, b, c to scene

(check-expect
 (add-triangle MT A B C)
 (scene+line
  (scene+line
   (scene+line
    MT (posn-x A) (posn-y A) (posn-x B) (posn-y B) COLOR)
   (posn-x B) (posn-y B) (posn-x C) (posn-y C) COLOR)
  (posn-x C) (posn-y C) (posn-x A) (posn-y A) COLOR))

(define (add-triangle scene a b c)
  (scene+line
   (scene+line
    (scene+line
     scene (posn-x a) (posn-y a) (posn-x b) (posn-y b) COLOR)
    (posn-x b) (posn-y b) (posn-x c) (posn-y c) COLOR)
   (posn-x c) (posn-y c) (posn-x a) (posn-y a) COLOR))
 
; Posn Posn Posn -> Boolean 
; is the triangle a, b, c too small to be divided

(check-expect (too-small? (make-posn 0 0)
                          (make-posn (/ THRESHOLD 2) (/ THRESHOLD 2))
                          (make-posn 0 (/ THRESHOLD 2)))
              #true)
(check-expect (too-small? (make-posn 0 0)
                          (make-posn (* THRESHOLD 2) (* THRESHOLD 2))
                          (make-posn 0 (* THRESHOLD 2)))
              #false)

(define (too-small? a b c)
  (or (< (distance a b) THRESHOLD)
      (< (distance b c) THRESHOLD)
      (< (distance c a) THRESHOLD)))
 
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