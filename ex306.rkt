;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex306) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; N -> [List-of N]
; creates the list (list 0 ... (- n 1)) for any natural number n

(check-expect (0-en 5)
              '(0 1 2 3 4))

(define (0-en n)
  (for/list ([i n]) i))

; N -> [List-of N]
; creates the list (list 1 ... n) for any natural number n

(check-expect (1-in 5)
              '(1 2 3 4 5))

(define (1-in n)
  (for/list ([i n]) (add1 i)))

; N -> [List-of Number]
; creates the list (list 1 1/2 ... 1/n) for any natural number n

(check-expect (1-1/n 5)
              '(1 1/2 1/3 1/4 1/5))

(define (1-1/n n)
  (for/list ([i n]) (/ 1 (add1 i))))

; N -> [List-of N]
; creates the list of the first n even numbers

(check-expect (even 5)
              '(0 2 4 6 8))

(define (even n)
  (for/list ([i (in-range 0 (* n 2) 2)]) i))

; N -> [List-of [List-of N]]
; creates a diagonal square of 0s and 1s

(check-expect (dsqr 5)
              '((1 0 0 0 0)
                (0 1 0 0 0)
                (0 0 1 0 0)
                (0 0 0 1 0)
                (0 0 0 0 1)))

(define (dsqr n)
  (for/list ([x n])
    (for/list ([y n])
      (if (= x y) 1 0))))

; [N -> Number] N -> [List-of Number]
; tabulates f between n and 0 (incl.) in a list

(check-expect (tabulate sqr 5)
              '(25 16 9 4 1 0))

(define (tabulate f n)
  (for/list ([i (add1 n)]) (f (- n i))))