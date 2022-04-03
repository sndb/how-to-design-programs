;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex323) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; BT Number -> [Maybe Symbol]
; if bt contains a node structure whose ssn field is n,
; produces the name in that node, #false otherwise

(check-expect (search-bt NONE 15) #false)
(check-expect (search-bt tree1 15) 'd)
(check-expect (search-bt tree2 86) #false)
(check-expect (search-bt tree2 87) 'h)

(define (search-bt bt n)
  (cond
    [(no-info? bt) #false]
    [else
     (if (= n (node-ssn bt))
         (node-name bt)
         (local ((define left-result
                   (search-bt (node-left bt) n)))
           (if (false? left-result)
               (search-bt (node-right bt) n)
               left-result)))]))