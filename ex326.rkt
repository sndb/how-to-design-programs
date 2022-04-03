;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex326) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

; A BST (short for binary search tree) is a BT according to
; the following conditions:
; — NONE is always a BST.
; — (make-node ssn0 name0 L R) is a BST if
;   — L is a BST,
;   — R is a BST,
;   — all ssn fields in L are smaller than ssn0,
;   — all ssn fields in R are larger than ssn0.

(define bst1 (make-node
              15
              'd
              NONE
              (make-node
               24 'i NONE NONE)))
     
(define bst2 (make-node
              63
              'e
              (make-node
               29
               'd
               (make-node
                15
                'b
                (make-node 10 'a NONE NONE)
                (make-node 24 'c NONE NONE))
               NONE)
              (make-node
               89
               'f
               (make-node 77 'e NONE NONE)
               (make-node
                95
                'h
                NONE
                (make-node 99 'i NONE NONE)))))

; BST Number Symbol -> BST
; produces a BST that is just like B and that in place of
; one NONE subtree contains the node structure
; (make-node N S NONE NONE)

(check-expect (create-bst NONE 1 'a)
              (make-node 1 'a NONE NONE))
(check-expect (create-bst bst1 10 'a)
              (make-node
               15
               'd
               (make-node 10 'a NONE NONE)
               (make-node
                24 'i NONE NONE)))
(check-expect (create-bst bst1 30 'a)
              (make-node
               15
               'd
               NONE
               (make-node
                24
                'i
                NONE
                (make-node
                 30 'a NONE NONE))))

(define (create-bst B N S)
  (cond
    [(no-info? B) (make-node N S NONE NONE)]
    [else
     (if (< N (node-ssn B))
         (make-node (node-ssn B)
                    (node-name B)
                    (create-bst (node-left B) N S)
                    (node-right B))
         (make-node (node-ssn B)
                    (node-name B)
                    (node-left B)
                    (create-bst (node-right B) N S)))]))