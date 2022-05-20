;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex513) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Lam is one of: 
; – Variable
; – Lambda
; – Application

(define-struct lmd (para body))
(define-struct app (fun arg))
; A Variable is a Symbol.
; A Lambda is a structure:
;   (make-lmd Variable Lam)
; An Application is a structure:
;   (make-app Lam Lam)

(define ex1 (make-lmd 'x 'x))
(define ex2 (make-lmd 'x 'y))
(define ex3 (make-lmd 'y (make-lmd 'x 'y)))
(define ex4 (make-app (make-lmd 'x (make-app 'x 'x))
                      (make-lmd 'x (make-app 'x 'x))))
(define ex5 (make-app (make-lmd 'x 'x) (make-lmd 'x 'x)))
(define ex6 (make-app (make-app (make-lmd 'y (make-lmd 'x 'y))
                                (make-lmd 'z 'z))
                      (make-lmd 'w 'w)))
(define ex7 'x)

; Lam -> [List-of Symbol]
; produces the list of all symbols used as λ parameters in a λ term

(check-expect (declareds ex1) '(x))
(check-expect (declareds ex2) '(x))
(check-expect (declareds ex3) '(y x))
(check-expect (declareds ex4) '(x x))
(check-expect (declareds ex5) '(x x))
(check-expect (declareds ex6) '(y x z w))
(check-expect (declareds ex7) '())

(define (declareds le)
  (cond
    [(symbol? le) '()]
    [(lmd? le)
     (cons (lmd-para le) (declareds (lmd-body le)))]
    [(app? le)
     (append (declareds (app-fun le)) (declareds (app-arg le)))]))