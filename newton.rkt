;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname newton) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ε 0.001)

; [Number -> Number] Number -> Number
; finds a number r such that (<= (abs (f r)) ε)
; generative repeatedly generates improved guesses
 
(check-within (newton poly 1) 2 ε)
(check-within (newton poly 3.5) 4 ε)
 
(define (newton f r1)
  (cond
    [(<= (abs (f r1)) ε) r1]
    [else (newton f (root-of-tangent f r1))]))
 
; [Number -> Number] Number -> Number
; computes the slope of f at r1

(check-expect (slope (lambda (x) (+ x 42)) 24) 1)
(check-expect (slope sqr 5) 10)

(define (slope f r1)
  (* (/ 1 (* 2 ε)) (- (f (+ r1 ε)) (f (- r1 ε)))))

; [Number -> Number] Number -> Number
; computes the root of the tangent through (r1,(f r1))

(check-expect (root-of-tangent (lambda (x) (+ x 42)) 24) -42)
(check-expect (root-of-tangent sqr 5) 2.5)

(define (root-of-tangent f r1)
  (- r1 (/ (f r1) (slope f r1))))

; Number -> Number
(define (poly x) (* (- x 2) (- x 4)))