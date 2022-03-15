;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex257) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X] N [N -> X] -> [List-of X]
; constructs a list by applying f to the numbers
; between 0 and (- n 1)

(check-expect (build-l*st 0 add1)
              (build-list 0 add1))
(check-expect (build-l*st 1 sub1)
              (build-list 1 sub1))
(check-expect (build-l*st 22 add1)
              (build-list 22 add1))

(define (build-l*st n f)
  (cond
    [(zero? n) '()]
    [else
     (add-at-end (build-l*st (sub1 n) f) (f (sub1 n)))]))

; [X] [List-of X] X -> [List-of X]
; adds x at the end of l
(define (add-at-end l x)
  (cond
    [(empty? l) (cons x '())]
    [else
     (cons (first l) (add-at-end (rest l) x))]))