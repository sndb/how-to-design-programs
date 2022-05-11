;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex462) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An SOE is a non-empty Matrix.
; constraint for (list r1 ... rn), (length ri) is (+ n 1)
; interpretation represents a system of linear equations
 
; An Equation is a [List-of Number].
; constraint an Equation contains at least two numbers. 
; interpretation if (list a1 ... an b) is an Equation, 
; a1, ..., an are the left-hand-side variable coefficients 
; and b is the right-hand side
 
; A Solution is a [List-of Number]
 
(define M ; an SOE 
  (list (list 2 2  3 10) ; an Equation 
        (list 2 5 12 31)
        (list 4 1 -2  1)))
 
(define S '(1 1 2)) ; a Solution

; Equation -> [List-of Number]
; extracts the left-hand side from a row in a matrix

(check-expect (lhs (first M)) '(2 2 3))

(define (lhs e)
  (reverse (rest (reverse e))))
 
; Equation -> Number
; extracts the right-hand side from a row in a matrix

(check-expect (rhs (first M)) 10)

(define (rhs e)
  (first (reverse e)))

; SOE Solution -> Boolean
; verifies the Solution s to the SOE m

(check-expect (check-solution M S) #true)
(check-expect (check-solution M '(1 2 2)) #false)

(define (check-solution m s)
  (local (; [List-of Number] -> Number
          (define (plug-numbers l)
            (foldr (lambda (i j a) (+ (* i j) a)) 0 l s))
          ; Equation -> Boolean
          (define (check-equation e)
            (= (plug-numbers (lhs e)) (rhs e))))
    (andmap check-equation m)))