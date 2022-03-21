;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex308) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

(define-struct phone [area switch four])
; A Phone is a structure:
;   (make-phone Number Number Number)

; [List-of Phone] -> [List-of Phone]

(define input `(,(make-phone 713 581 8432)
                ,(make-phone 275 829 1238)))
(define expect `(,(make-phone 281 581 8432)
                 ,(make-phone 275 829 1238)))

(check-expect (replace input) expect)

(define (replace l)
  (for/list ([p l])
    (match p
      [(phone 713 y z) (make-phone 281 y z)]
      [else else])))