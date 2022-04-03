;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex327) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
              'a
              (make-node
               29
               'b
               (make-node
                15
                'd
                (make-node 10 'h NONE NONE)
                (make-node 24 'i NONE NONE))
               NONE)
              (make-node
               89
               'c
               (make-node 77 'l NONE NONE)
               (make-node
                95
                'g
                NONE
                (make-node 99 'o NONE NONE)))))

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

; [List-of [List Number Symbol]] -> BST
; produces a BST by repeatedly applying create-bst

(check-expect
 (create-bst-from-list '((99 o)
                         (77 l)
                         (24 i)
                         (10 h)
                         (95 g)
                         (15 d)
                         (89 c)
                         (29 b)
                         (63 a)))
 bst2)

(define (create-bst-from-list l)
  (foldr (lambda (ns bst)
           (create-bst bst (first ns) (second ns)))
         NONE l))