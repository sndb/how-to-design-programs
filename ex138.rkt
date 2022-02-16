;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex138) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-amounts is one of:
; – '()
; – (cons PositiveNumber List-of-amounts)
(define ex1 '())
(define ex2 (cons 1 '()))
(define ex3 (cons 2 (cons 1 '())))

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

(define l (cons 27 (cons 64 (cons 49 '()))))
(sum l)