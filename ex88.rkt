;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex88) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A VCat is a structure:
;   (make-vcat Number Number)
; interpretation: represents the cat's x-coordinate
; and its happiness
(define-struct vcat [x happiness])