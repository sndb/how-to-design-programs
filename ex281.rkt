;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex281) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define DOT (circle 10 'solid 'red))

(define-struct IR [name price])

(lambda (x) (< x 10))
(lambda (x y) (number->string (* x y)))
(lambda (x) (if (even? x) 0 1))
(lambda (x y) (> (IR-price x) (IR-price y)))
(lambda (p img) (place-image DOT (posn-x p) (posn-y p) img))