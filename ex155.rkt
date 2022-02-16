;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex155) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct layer [color doll])
; An RD (short for Russian doll) is one of:
; – String
; – (make-layer String RD)

; RD -> String
; produces the color of the innermost doll

(check-expect (inner "red") "red")
(check-expect
 (inner (make-layer "yellow" (make-layer "green" "red")))
 "red")

(define (inner an-rd)
  (cond
    [(string? an-rd) an-rd]
    [(layer? an-rd) (inner (layer-doll an-rd))]))