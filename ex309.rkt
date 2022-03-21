;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex309) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; [List-of [List-of String]] -> N
; determines the number of Strings per item in a list of
; list of strings

(check-expect (words-on-line '(("hello")
                               ("hello" "world")))
              '(1 2))

(define (words-on-line lls)
  (match lls
    ['() '()]
    [(cons fst rst)
     (cons (length fst) (words-on-line rst))]))