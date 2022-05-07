;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex437) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (special P)
  (cond
    [(empty? P) (solve P)]
    [else
     (combine-solutions
      P
      (special (rest P)))]))

;; special computes the length of its input

(check-expect (special '()) 0)
(check-expect (special '(a b c)) 3)

(define (solve P)
  0)

(define (combine-solutions P Q)
  (+ 1 Q))

;; special negates each number on the given list of numbers

(check-expect (special '()) '())
(check-expect (special '(1 -4 3)) '(-1 4 -3))

(define (solve P)
  '())

(define (combine-solutions P Q)
  (cons (- (first P)) Q))

;; special uppercases the given list of strings

(check-expect (special '()) '())
(check-expect (special '("hello" "world")) '("HELLO" "WORLD"))

(define (solve P)
  '())

(define (combine-solutions P Q)
  (cons (string-upcase (first P)) Q))