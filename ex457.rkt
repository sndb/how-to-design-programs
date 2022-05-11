;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex457) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number Number -> N
; computes how many iterations it takes to double n1 increasing in r at
; each iteration

(check-expect (double-amount 100 2) 1)
(check-expect (double-amount 100 1.5) 2)

(define (double-amount n1 r)
  (local (; Number N -> N
          (define (double-amount-helper n)
            (cond
              [(>= n (* n1 2)) 0]
              [else (add1 (double-amount-helper (* n r)))])))
    (double-amount-helper n1)))