;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex471) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Node is a Symbol.

; A Graph is a [List-of (cons Node [List-of Nodes])].
; interpretation: a list of nodes with their neighbors

(define sample-graph
  (list (list 'A 'B 'E)
        (list 'B 'E 'F)
        (list 'C 'D)
        (list 'D)
        (list 'E 'C 'F)
        (list 'F 'D 'G)
        (list 'G)))

; Node Graph -> [List-of Nodes]
; produces the list of immediate neighbors of n in g

(check-expect (neighbors 'A sample-graph) '(B E))
(check-expect (neighbors 'D sample-graph) '())
(check-error (neighbors 'H sample-graph) "node not found")

(define (neighbors n g)
  (cond
    [(empty? g) (error "node not found")]
    [else
     (local ((define c (first g)))
       (if (symbol=? n (first c))
           (rest c)
           (neighbors n (rest g))))]))