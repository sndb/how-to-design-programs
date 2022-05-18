;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex505) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; N [>=1] -> Boolean
; determines whether n is a prime number

(check-expect (is-prime? 2) #true)
(check-expect (is-prime? 3) #true)
(check-expect (is-prime? 4) #false)
(check-expect (is-prime? 7) #true)
(check-expect (is-prime? 15) #false)
(check-expect (is-prime? 97) #true)

(define (is-prime? n0)
  (local (; N -> Boolean
          (define (is-prime?/a n)
            (cond
              [(= n 1) #true]
              [(= (remainder n0 n) 0) #false]
              [else (is-prime?/a (sub1 n))])))
    (is-prime?/a (sub1 n0))))