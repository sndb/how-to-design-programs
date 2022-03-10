;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex252) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; [List-of Number] -> Number
(define (product l)
  (cond
    [(empty? l) 1]
    [else
     (* (first l)
        (product (rest l)))]))
  
; [List-of Posn] -> Image
(define (image* l)
  (cond
    [(empty? l) emt]
    [else
     (place-dot (first l)
                (image* (rest l)))]))
 
; Posn Image -> Image 
(define (place-dot p img)
  (place-image dot (posn-x p) (posn-y p) img))
 
; graphical constants:    
(define emt (empty-scene 100 100))
(define dot (circle 3 "solid" "red"))

; [X R -> R] R [List-of X] -> R

(check-expect
 (fold2 * 1 '(1 2 3))
 (product '(1 2 3)))

(check-expect
 (fold2 place-dot emt `(,(make-posn 1 2)
                        ,(make-posn 2 3)
                        ,(make-posn 3 4)))
 (image* `(,(make-posn 1 2)
           ,(make-posn 2 3)
           ,(make-posn 3 4))))

(define (fold2 f b l)
  (cond
    [(empty? l) b]
    [else
     (f (first l)
        (fold2 f b (rest l)))]))