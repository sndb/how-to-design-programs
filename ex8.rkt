;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define cat (rectangle 20 20 "solid" "black"))

(define label
  (if (>= (image-height cat) (image-width cat))
      "tall"
      "wide"))

(define label2
  (cond [(> (image-height cat) (image-width cat)) "tall"]
        [(< (image-height cat) (image-width cat)) "wide"]
        [else "square"]))
  
label
label2