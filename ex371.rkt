;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex371) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An Xexpr is a list:
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

(define a0 '((initial "X")))
 
(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))

; An XWord is '(word ((text String))).

(define w0 '(word ((text ""))))
(define w1 '(word ((text "hello"))))
(define w2 '(word ((text "hello world"))))

; Exercise:
; Refine the definition of Xexpr so that you can represent
; XML elements, including items in enumerations, that are
; plain strings.

; Solution:
; XWord is an Xexpr, so Xexpr can already represent XML
; elements that are plain strings. Nothing to be done here.