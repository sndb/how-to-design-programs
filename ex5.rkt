;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define s 3)

(define sail
  (overlay/offset (star-polygon 8 7 3 "solid" "gold")
                  5 -10
                  (right-triangle 32 48 "solid" "gray")))
(define rs
  (flip-vertical (right-triangle 32 32 "solid" "brown")))
(define ls (flip-horizontal rs))
(define base (rectangle 48 32 "solid" "brown"))
(define boat (beside ls base rs))

(scale s (above sail boat))