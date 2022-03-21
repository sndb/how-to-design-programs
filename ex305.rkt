;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex305) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

(define $-per-€ 1.06)

; [List-of Number] -> [List-of Number]
; converts a list of US$ amounts into a list of € amounts

(check-expect (convert-euro '(1 2 3))
              '(1.06 2.12 3.18))

(define (convert-euro l)
  (for/list ([v l])
    (* v $-per-€)))