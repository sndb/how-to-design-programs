;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex78) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A TL-Word is a structure:
;   (make-tl-word Letter Letter Letter)
; A Letter is one of:
; — 1String "a" through "z"
; — #false
; interpretation: three-letter word
(define-struct tl-word [first second third])