;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex499) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] -> Number
; computes the product of l0

(check-expect (product '()) 1)
(check-expect (product '(1 2 3 4)) 24)

(define (product l0)
  (local (; [List-of Number] Number -> Number
          ; computes the product of l0
          ; accumulator a is the product of numbers that are not in l
          ; but in l0
          (define (product/a l a)
            (cond
              [(empty? l) a]
              [else
               (product/a (rest l) (* a (first l)))])))
    (product/a l0 1)))

; Q: The performance of product is O(n) where n is the length of the
; list. Does the accumulator version improve on this?
; A: No. The accumulator version still needs to traverse the list
; once.