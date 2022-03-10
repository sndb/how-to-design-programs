;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex255) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] [Number -> Number] -> [List-of Number]
(define (map-n l f) l)

; [List-of String] [String -> String] -> [List-of String]
(define (map-s l f) l)

; [X Y] [List-of X] [X -> Y] -> [List-of Y]