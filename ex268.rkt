;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex268) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct IR [name desc acqprice salesprice])
; An IR is a structure:
;   (make-IR String String Number Number)
; interpretation: an inventory record specifies the name of
; an item, a description, the acquisition price, and the
; recommended sales price

; [List-of IR] -> [List-of IR]
; sorts a list of inventory records by the difference
; between the two prices with >

(check-expect (sort-by-diff `(,(make-IR "a" "abc" 10 20)
                              ,(make-IR "b" "bcd" 20 50)
                              ,(make-IR "c" "cde" 40 45)))
              `(,(make-IR "b" "bcd" 20 50)
                ,(make-IR "a" "abc" 10 20)
                ,(make-IR "c" "cde" 40 45)))

(define (sort-by-diff l)
  (local (; IR IR -> Boolean
          (define (cmp-diff-> a b)
            (> (price-diff a) (price-diff b))))
    (sort l cmp-diff->)))

; IR -> Number
; computes the difference between the two prices

(check-expect (price-diff (make-IR "a" "abc" 10 20)) 10)

(define (price-diff x)
  (abs (- (IR-acqprice x) (IR-salesprice x))))