;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex267) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define $-per-€ 1.06)

; [List-of Number] -> [List-of Number]
; converts a list of US$ amounts into a list of € amounts

(check-expect (convert-euro '(1 2 3))
              '(1.06 2.12 3.18))

(define (convert-euro usl)
  (local (; Number -> Number
          (define ($-to-€ x)
            (* x $-per-€)))
    (map $-to-€ usl)))

; [List-of Number] -> [List-of Number]
; converts a list of Fahrenheit measurements to a list of
; Celsius measurements

(check-within (convertFC '(32 59 98))
              '(0 15 36.6)
              0.1)

(define (convertFC lf)
  (local (; Number -> Number
          (define (F->C f)
            (* 5/9 (- f 32))))
    (map F->C lf)))

; [List-of Posn] -> [List-of [List-of Number]]
; translates a list of Posns into a list of lists of pairs
; of numbers

(check-expect (translate `(,(make-posn 0 0)
                           ,(make-posn 3 4)
                           ,(make-posn 5 7)))
              '((0 0) (3 4) (5 7)))

(define (translate l)
  (local (; Posn -> [List-of Number]
          (define (p->l p)
            `(,(posn-x p) ,(posn-y p))))
    (map p->l l)))