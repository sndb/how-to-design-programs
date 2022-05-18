;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex500) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of X] -> N
; determines the number of items on a list

(check-expect (how-many '()) 0)
(check-expect (how-many '(1 2 3)) 3)
(check-expect (how-many '(hello world)) 2)

(define (how-many l)
  (local (; [List-of X] N -> N
          ; accumulator a is the number of elements
          ; that l lacks from l0
          (define (how-many/a l a)
            (cond
              [(empty? l) a]
              [else
               (how-many/a (rest l) (add1 a))])))
    (how-many/a l 0)))

; Q: The performance of how-many is O(n) where n is the length of the
; list. Does the accumulator version improve on this?
; A: No. The preformance of accumulator version is still O(n).

; Q: Does the accumulator reduce the amount of space needed to compute
; the result?
; A: Yes. It reduces the amount of space to O(1) - there is no pending
; function applications at all.