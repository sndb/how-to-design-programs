;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex504) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Digit is a natural number in the interval [0,9].

; [List-of Digit] -> Number
; produces the corresponding number

(check-expect (to10 '(1 0 2)) 102)

(define (to10 l0)
  (local (; [List-of Digit] Number -> Number
          ; accumulator a is the number containing corresponding
          ; digits that l lacks from l0
          (define (to10/a l a)
            (cond
              [(empty? l) a]
              [else
               (to10/a (rest l) (+ (* 10 a) (first l)))])))
    (to10/a l0 0)))