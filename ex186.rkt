;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex186) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-numbers is one of:
; – '()
; – (cons Number List-of-numbers)

; List-of-numbers -> List-of-numbers
; produces a sorted version of l

(check-expect (sort> '()) '())
(check-satisfied (sort> (list 3 2 1)) sorted>?)
(check-satisfied (sort> (list 1 2 3)) sorted>?)
(check-satisfied (sort> (list 12 20 -5)) sorted>?)

(define (sort> l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort> (rest l)))]))

; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers l

(check-satisfied (insert 5 '()) sorted>?)
(check-satisfied (insert 5 (list 6)) sorted>?)
(check-satisfied (insert 5 (list 4)) sorted>?)
(check-satisfied (insert 12 (list 20 -5)) sorted>?)
(check-satisfied (insert 3 (list 2 1)) sorted>?)
(check-satisfied (insert 1 (list 3 2)) sorted>?)

(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (>= n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))

; List-of-numbers -> Boolean
; determines if l is sorted in descending order

(check-expect
 (sorted>? (list 1 2 3)) #false)
(check-expect
 (sorted>? (list 3 1 2)) #false)
(check-expect
 (sorted>? (list 3 2 1)) #true)

(define (sorted>? l)
  (cond
    [(empty? (rest l)) #true]
    [else
     (and (>= (first l) (second l))
          (sorted>? (rest l)))]))

; List-of-numbers -> List-of-numbers
; produces a sorted version of l

(check-expect (sort>/bad (list 3 1 2)) (list 3 2 1))
(check-satisfied (sort>/bad (list 3 1 2)) sorted>?)

(define (sort>/bad l)
  (list 9 8 7 6 5 4 3 2 1 0))