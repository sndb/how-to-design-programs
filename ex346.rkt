;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex346) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A BSL-expr is one of:
; — Addition
; — Multiplication
; — Value

(define-struct add [left right])
; An Addition is a structure:
;   (make-add BSL-expr BSL-expr)
; interpretation: (make-add 2 2) represents an expression (+ 2 2)

(define-struct mul [left right])
; A Multiplication is a structure:
;   (make-mul BSL-expr BSL-expr)
; interpretation: (make-mul 2 2) represents an expression (+ 2 2)

; A Value is a Number.

; Examples
(define expr-3 3)
(define expr-2 (make-add 1 1))
(define expr-30 (make-mul 3 10))
(define expr-11 (make-add (make-mul 1 1) 10))