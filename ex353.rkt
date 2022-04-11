;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex353) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A BSL-expr is one of:
; — Value
; — Addition
; — Multiplication

; A Value is a Number.

(define-struct add [left right])
; An Addition is a structure:
;   (make-add BSL-expr BSL-expr)
; interpretation: (make-add 2 2) represents an expression (+ 2 2)

(define-struct mul [left right])
; A Multiplication is a structure:
;   (make-mul BSL-expr BSL-expr)
; interpretation: (make-mul 2 2) represents an expression (+ 2 2)

(define expr-3 3)
(define expr-2 (make-add 1 1))
(define expr-30 (make-mul 3 10))
(define expr-11 (make-add (make-mul 1 1) 10))

; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

(define x 5)
(define y 3)

(define expr-5
  'x)
(define expr-8
  (make-add 'x 3))
(define expr-7.5
  (make-mul 1/2 (make-mul 'x 3)))
(define expr-34
  (make-add (make-mul 'x 'x)
            (make-mul 'y 'y)))

; BSL-var-expr -> Boolean
; determines whether ex is also a BSL-expr

(check-expect (numeric? expr-30) #true)
(check-expect (numeric? expr-11) #true)
(check-expect (numeric? expr-5) #false)
(check-expect (numeric? expr-7.5) #false)

(define (numeric? ex)
  (cond
    [(number? ex) #true]
    [(symbol? ex) #false]
    [(add? ex) (and (numeric? (add-left ex))
                    (numeric? (add-right ex)))]
    [(mul? ex) (and (numeric? (mul-left ex))
                    (numeric? (mul-right ex)))]))