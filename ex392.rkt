;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex392) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct branch [left right])
 
; A TOS is one of:
; – Symbol
; – (make-branch TOS TOS)
 
; A Direction is one of:
; – 'left
; – 'right
 
; A list of Directions is also called a path.

; TOS [List-of Direction] -> Symbol
; picks the symbol after following a path

(check-expect (tree-pick (make-branch (make-branch 'a 'b) 'c)
                         '(left right)) 'b)
(check-error (tree-pick (make-branch (make-branch 'a 'b) 'c)
                        '(left))
             "path is incorrect")
(check-error (tree-pick 'x '(left))
             "path is incorrect")
(check-expect (tree-pick 'x '()) 'x)

(define (tree-pick tree path)
  (cond
    [(and (symbol? tree) (empty? path)) tree]
    [(and (branch? tree) (cons? path))
     (tree-pick
      (if (symbol=? 'left (first path))
          (branch-left tree)
          (branch-right tree))
      (rest path))]
    [else (error "path is incorrect")]))