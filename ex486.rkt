;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex486) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define bigEnough 1)
(define c 2)

(define (f n)
  (+ (* n n) n))

(define (n2 n)
  (* n n))

(<= (f bigEnough) (* c (n2 bigEnough)))