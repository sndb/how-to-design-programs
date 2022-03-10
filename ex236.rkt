;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex236) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Lon -> Lon
; adds 1 to each item on l

(check-expect (add1* '()) '())
(check-expect (add1* '(1)) '(2))
(check-expect (add1* '(1 3 5)) '(2 4 6))

(define (add1* l)
  (plusn 1 l))
     
; Lon -> Lon
; adds 5 to each item on l

(check-expect (plus5 '()) '())
(check-expect (plus5 '(1)) '(6))
(check-expect (plus5 '(1 3 5)) '(6 8 10))

(define (plus5 l)
  (plusn 5 l))

; Lon -> Lon
; subtracts 2 from each item on l

(check-expect (sub2 '()) '())
(check-expect (sub2 '(1)) '(-1))
(check-expect (sub2 '(1 3 5)) '(-1 1 3))

(define (sub2 l)
  (plusn -2 l))

; Number Lon
; adds n to each item on l
(define (plusn n l)
  (cond
    [(empty? l) '()]
    [else
     (cons
      (+ n (first l))
      (plusn n (rest l)))]))