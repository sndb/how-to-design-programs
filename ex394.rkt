;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex394) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] [List-of Number] -> [List-of Number]
; produces a single sorted list of numbers that contains all
; the numbers on both inputs lists
; assume input lists are sorted in ascending order

(check-expect (merge '() '()) '())
(check-expect (merge '() '(2 4 5)) '(2 4 5))
(check-expect (merge '(1 4 6) '()) '(1 4 6))
(check-expect (merge '(1 4 6) '(2 4 5)) '(1 2 4 4 5 6))
(check-expect (merge '(-3 0 1 4 6) '(2 4 5)) '(-3 0 1 2 4 4 5 6))

(define (merge l1 l2)
  (cond
    [(empty? l1) l2]
    [(empty? l2) l1]
    [else
     (if (< (first l1) (first l2))
         (cons (first l1) (merge (rest l1) l2))
         (cons (first l2) (merge l1 (rest l2))))]))