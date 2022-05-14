;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex488) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define bigEnough 1)
(define c 1)

(define (f n)
  (* n (log n 2)))

(define (g n)
  (* n n))

(<= (f bigEnough) (* c (g bigEnough)))

; Q: Does f belong to O(g) or g to O(f)?
; A: f belong to O(g).