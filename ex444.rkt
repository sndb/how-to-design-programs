;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex444) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; N[>= 1] N[>= 1] -> N
; finds the greatest common divisor of n and m

(check-expect (gcd-structural 6 25) 1)
(check-expect (gcd-structural 18 24) 6)

(define (gcd-structural S L)
  (largest-common (divisors S S) (divisors S L)))
 
; N[>= 1] N[>= 1] -> [List-of N]
; computes the divisors of l smaller or equal to k

(check-expect (divisors 7 16) '(1 2 4))
(check-expect (divisors 8 16) '(1 2 4 8))

(define (divisors k l)
  (filter (lambda (x) (= 0 (remainder l x)))
          (build-list k add1)))
 
; [List-of N] [List-of N] -> N
; finds the largest number common to both k and l

(check-expect (largest-common '(1 2 4 8)
                              '(1 2 4 7 11))
              4)

(define (largest-common k l)
  (local ((define common-numbers
            (filter (lambda (x) (member? x l)) k))
          (define largest-number
            (argmax identity common-numbers)))
    largest-number))