;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex287) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct IR [name desc acqprice salesprice])
; An IR is a structure:
;   (make-IR String String Number Number)
; interpretation: an inventory record specifies the name of
; an item, a description, the acquisition price, and the
; recommended sales price

; Number [List-of IR] -> [List-of IR]
; produces a list of all those structures whose acquisition
; price is below ua

(check-expect (eliminate-exp
               20
               `(,(make-IR "a" "abc" 10 20)
                 ,(make-IR "b" "bcd" 20 50)
                 ,(make-IR "c" "cde" 40 45)))
              `(,(make-IR "a" "abc" 10 20)))

(define (eliminate-exp ua l)
  (filter (lambda (r) (< (IR-acqprice r) ua)) l))

; String [List-of IR] -> [List-of IR]
; produces a list of inventory records that do not use the
; name ty

(check-expect (recall "a"
                      `(,(make-IR "a" "abc" 10 20)
                        ,(make-IR "b" "bcd" 20 50)
                        ,(make-IR "c" "cde" 40 45)))
              `(,(make-IR "b" "bcd" 20 50)
                ,(make-IR "c" "cde" 40 45)))

(define (recall ty l)
  (filter (lambda (r) (not (string=? ty (IR-name r)))) l))

; [List-of String] [List-of String] -> [List-of String]
; selects all those names from l2 that are also on l1

(check-expect (selection '("a" "b" "c" "d")
                         '("b" "c" "x" "y"))
              '("b" "c"))

(define (selection l1 l2)
  (filter (lambda (n) (member? n l1)) l2))