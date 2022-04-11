;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex356) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct fun [name arg])
(define-struct add [left right])
(define-struct mul [left right])
; A BSL-fun-expr is one of: 
; – Number
; – Symbol
; — (make-fun Symbol BSL-fun-expr)
; – (make-add BSL-fun-expr BSL-fun-expr)
; – (make-mul BSL-fun-expr BSL-fun-expr)

; BSL-fun-expr examples
;(k (+ 1 1))
(define k2 (make-fun 'k (make-add 1 1)))
;(* 5 (k (+ 1 1)))
(define 5k2 (make-mul 5 k2))
;(* (i 5) (k (+ 1 1)))
(define i5k2 (make-mul (make-fun 'i 5) k2))