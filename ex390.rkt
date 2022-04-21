;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex390) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct branch [left right])
 
; A TOS is one of:
; – Symbol
; – (make-branch TOS TOS)
 
; A Direction is one of:
; – 'left
; – 'right
 
; A list of Directions is also called a path. 

; [List-of Symbol] N -> Symbol
; extracts the nth symbol from l; 
; signals an error if there is no such symbol

(check-expect (list-pick '(a b c) 2) 'c)
(check-error (list-pick '() 0) "list-pick: list too short")
(check-expect (list-pick (cons 'a '()) 0) 'a)
(check-error (list-pick '() 3) "list-pick: list too short")

(define (list-pick l n)
  ; designed in a systematic manner
  ;  (cond
  ;    [(and (= n 0) (empty? l))
  ;     (error 'list-pick "list too short")]
  ;    [(and (> n 0) (empty? l))
  ;     (error 'list-pick "list too short")]
  ;    [(and (= n 0) (cons? l)) (first l)]
  ;    [(and (> n 0) (cons? l)) (list-pick (rest l) (sub1 n))]))
  ; simplified
  (cond
    [(empty? l) (error 'list-pick "list too short")]
    [(= n 0) (first l)]
    [(> n 0) (list-pick (rest l) (sub1 n))]))

; TOS [List-of Direction] -> Symbol
; picks the symbol after following a path

(check-expect (tree-pick (make-branch (make-branch 'a 'b) 'c)
                         '(left right)) 'b)
(check-error (tree-pick (make-branch (make-branch 'a 'b) 'c)
                        '(left)))
(check-error (tree-pick 'x '(left)))
(check-expect (tree-pick 'x '()) 'x)

(define (tree-pick tree path)
  (cond
    [(and (symbol? tree) (empty? path)) tree]
    [(and (symbol? tree) (cons? path))
     (error "tree is symbol but path is not empty")]
    [(and (branch? tree) (empty? path))
     (error "empty path but tree is not symbol")]
    [(and (branch? tree) (cons? path))
     (tree-pick (if (symbol=? 'left (first path))
                    (branch-left tree)
                    (branch-right tree))
                (rest path))]))