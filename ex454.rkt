;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex454) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; N [List-of Number] -> [List-of [List-of Number]]
; produces an n x n matrix
; assume: the length of l is n^2

(check-expect
 (create-matrix 2 (list 1 2 3 4))
 (list (list 1 2)
       (list 3 4)))

(check-expect
 (create-matrix 3 (list 1 2 3 4 5 6 7 8 9))
 (list (list 1 2 3)
       (list 4 5 6)
       (list 7 8 9)))

(define (create-matrix n l)
  (cond
    [(empty? l) '()]
    [else
     (cons (take l n) (create-matrix n (drop l n)))]))

; [X] [List-of X] N -> [List-of X]
; takes n first items from l or all of them if it is too short

(check-expect (take '() 0) '())
(check-expect (take '() 3) '())
(check-expect (take '(4 2 5 6) 0) '())
(check-expect (take '(4 2 5 6) 3) '(4 2 5))

(define (take l n)
  (cond
    [(or (empty? l) (zero? n)) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))

; [X] [List-of X] N -> [List-of X]
; removes n first items from l or all of them if it is too short

(check-expect (drop '() 0) '())
(check-expect (drop '() 3) '())
(check-expect (drop '(4 2 5 6) 0) '(4 2 5 6))
(check-expect (drop '(4 2 5 6) 3) '(6))

(define (drop l n)
  (cond
    [(or (empty? l) (zero? n)) l]
    [else (drop (rest l) (sub1 n))]))