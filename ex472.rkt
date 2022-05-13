;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex472) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false

(check-expect (find-path 'A 'G sample-graph)
              '(A B E F G))

(define (find-path origination destination G)
  (cond
    [(symbol=? origination destination) (list destination)]
    [else (local ((define next (neighbors origination G))
                  (define candidate
                    (find-path/list next destination G)))
            (cond
              [(boolean? candidate) #false]
              [else (cons origination candidate)]))]))
 
; [List-of Node] Node Graph -> [Maybe Path]
; finds a path from some node on lo-Os to D
; if there is no path, the function produces #false
(define (find-path/list lo-Os D G)
  (cond
    [(empty? lo-Os) #false]
    [else (local ((define candidate
                    (find-path (first lo-Os) D G)))
            (cond
              [(boolean? candidate)
               (find-path/list (rest lo-Os) D G)]
              [else candidate]))]))

; Graph -> Boolean
; determines whether there is a path between every pair of nodes

(check-expect (test-on-all-nodes sample-graph) #false)

(define (test-on-all-nodes g)
  (andmap (lambda (O-D) (cons? (find-path (first O-D) (second O-D) g)))
          (all-pairs g)))

; Graph -> [List-of [List Node Node]]
; produces all node pairs of g

(check-expect (all-pairs '((A B C) (B) (C)))
              '((A A) (A B) (A C) (B A) (B B) (B C) (C A) (C B) (C C)))

(define (all-pairs g)
  (foldr (lambda (x a)
           (append (map (lambda (y) (list (first x) (first y))) g) a))
         '()
         g))