;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex238) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Nelon -> Number
; determines the smallest
; number on l
(define (inf l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (min (first l) (inf (rest l)))]))

; Nelon -> Number
; determines the largest 
; number on l
(define (sup l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (max (first l) (sup (rest l)))]))

(define (est F l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (F (first l) (est F (rest l)))]))

(check-expect
 (inf-1 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
              12 11 10 9 8 7 6 5 4 3 2 1))
 (inf (list 25 24 23 22 21 20 19 18 17 16 15 14 13
            12 11 10 9 8 7 6 5 4 3 2 1)))

(check-expect
 (inf-1 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
              17 18 19 20 21 22 23 24 25))
 (inf (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
            17 18 19 20 21 22 23 24 25)))

(define (inf-1 l)
  (est min l))

(check-expect
 (sup-1 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
              12 11 10 9 8 7 6 5 4 3 2 1))
 (sup (list 25 24 23 22 21 20 19 18 17 16 15 14 13
            12 11 10 9 8 7 6 5 4 3 2 1)))

(check-expect
 (sup-1 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
              17 18 19 20 21 22 23 24 25))
 (sup (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
            17 18 19 20 21 22 23 24 25)))

(define (sup-1 l)
  (est max l))