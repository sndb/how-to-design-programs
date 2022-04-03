;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex324) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

(define tree1 (make-node
               15
               'd
               NONE
               (make-node
                24 'i NONE NONE)))
     
(define tree2 (make-node
               15
               'd
               (make-node
                87 'h NONE NONE)
               NONE))

; BT -> [List-of Number]
; produces the sequence of all the ssn numbers in bt as they
; show up from left to right when looking at a tree drawing

(check-expect (inorder NONE) '())
(check-expect (inorder tree1) '(15 24))
(check-expect (inorder tree2) '(87 15))

(define (inorder bt)
  (cond
    [(no-info? bt) '()]
    [else
     (append (inorder (node-left bt))
             `(,(node-ssn bt))
             (inorder (node-right bt)))]))