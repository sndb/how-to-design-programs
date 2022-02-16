;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex139) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-numbers is one of:
; – '()
; – (cons Number List-of-numbers)

; A List-of-amounts is one of:
; – '()
; – (cons PositiveNumber List-of-amounts)
(define ex1 '())
(define ex2 (cons 1 '()))
(define ex3 (cons 2 (cons 1 '())))

; List-of-numbers -> Boolean
; determines whether all numbers are positive numbers
(check-expect (pos? '()) #true)
(check-expect (pos? (cons 5 '())) #true)
(check-expect (pos? (cons 5 (cons 3 '()))) #true)
(check-expect (pos? (cons -1 '())) #false)
(check-expect (pos? (cons 5 (cons 0 '()))) #false)
(define (pos? l)
  (cond
    [(empty? l) #true]
    [else
     (and (> (first l) 0)
          (pos? (rest l)))]))

; List-of-numbers -> Number
; computes the sum of l if the input belong to List-of-amounts;
; otherwise signals an error
(check-expect (checked-sum '()) 0)
(check-expect (checked-sum (cons 5 '())) 5)
(check-expect (checked-sum (cons 5 (cons 3 '()))) 8)
(check-error (checked-sum (cons -1 '())))
(check-error (checked-sum (cons 5 (cons 0 '()))))
(define (checked-sum l)
  (cond
    [(pos? l) (sum l)]
    [else (error "sum: List-of-amounts expected")]))

; List-of-amounts -> Number
; computes the sum of the amounts
(check-expect (sum ex1) 0)
(check-expect (sum ex2) 1)
(check-expect (sum ex3) 3)
(define (sum amounts)
  (cond
    [(empty? amounts) 0]
    [else
     (+ (first amounts)
        (sum (rest amounts)))]))