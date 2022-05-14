;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex485) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A NumberTree is one of:
; — Number
; — [List NumberTree NumberTree]

; NumberTree -> Number
; determines the sum of the numbers in a tree

(check-expect (sum-tree 4) 4)
(check-expect (sum-tree '(4 2)) 6)
(check-expect (sum-tree '(4 ((1 7) 2))) 14)

(define (sum-tree t)
  (cond
    [(number? t) t]
    [else
     (+ (sum-tree (first t)) (sum-tree (second t)))]))

; Q: What is its abstract running time?
; A: On the order of n steps.
; Q: What is an acceptable measure of the size of such a tree?
; A: A number of elements inside a tree.
; Q: What is the worst possible shape of the tree? What’s the best
; possible shape?
; A: There will be equal amount of recursive steps for a tree of N
; numbers, regardless of its structure.