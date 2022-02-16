;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex157) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 220) ; distances in terms of pixels
(define WIDTH 30)
(define XSHOTS (/ WIDTH 3))

; graphical constants
(define BACKGROUND (empty-scene WIDTH HEIGHT "green"))
(define SHOT (rectangle 6 18 "solid" "black"))

; A List-of-numbers is one of:
; – '()
; – (cons Number List-of-numbers)

; A ShotWorld is List-of-numbers.
; interpretation: each number on such a list
;   represents the y-coordinate of a shot

; ShotWorld -> ShotWorld
(define (main w0)
  (big-bang w0
    [on-tick tock]
    [on-key keyh]
    [to-draw to-image]))

; ShotWorld -> ShotWorld
; moves each shot up by one pixel

(check-expect (tock '()) '())
(check-expect (tock (cons 7 '())) (cons 6 '()))
(check-expect (tock (cons 7 (cons 42 '()))) (cons 6 (cons 41 '())))

(define (tock w)
  (cond
    [(empty? w) '()]
    [else (cons (sub1 (first w)) (tock (rest w)))]))

; ShotWorld KeyEvent -> ShotWorld
; adds a shot to the world if the space bar is hit

(check-expect (keyh '() "a") '())
(check-expect (keyh '() " ") (cons HEIGHT '()))
(check-expect (keyh (cons 42 '()) " ") (cons HEIGHT (cons 42 '())))

(define (keyh w ke)
  (if (key=? ke " ") (cons HEIGHT w) w))

; ShotWorld -> Image
; adds each shot y on w at (XSHOTS,y) to BACKGROUND

(check-expect (to-image '()) BACKGROUND)
(check-expect
 (to-image (cons 9 '()))
 (place-image SHOT XSHOTS 9 BACKGROUND))
(check-expect
 (to-image (cons 3 (cons 9 '())))
 (place-image SHOT XSHOTS 3 (place-image SHOT XSHOTS 9 BACKGROUND)))

(define (to-image w)
  (cond
    [(empty? w) BACKGROUND]
    [else (place-image SHOT XSHOTS (first w)
                       (to-image (rest w)))]))