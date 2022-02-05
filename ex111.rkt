;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex111) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct vec [x y])
; A Vec is a structure:
;   (make-vec PositiveNumber PositiveNumber)
; interpretation: represents a velocity vector

; Any Any -> Vec
; makes a new velocity vector, if x and y are positive numbers
(define (checked-make-vec x y)
  (cond
    [(and (real? x) (positive? x)
          (real? y) (positive? y))
     (make-vec x y)]
    [else (error "make-vec: positive numbers expected")]))