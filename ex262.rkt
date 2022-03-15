;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex262) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; N -> [List-of [List-of Number]]
; creates a diagonal square of 0s and 1s of size n

(check-expect (identityM 0)
              '())
(check-expect (identityM 1)
              '((1)))
(check-expect (identityM 3)
              '((1 0 0)
                (0 1 0)
                (0 0 1)))

(define (identityM n)
  (local (; N -> [List-of Number]
          ; places 1 at position x, 0 at others
          (define (1-at-x x)
            (local (; N -> Number
                    ; 1 if v equals x, 0 otherwise
                    (define (x? v)
                      (if (= x v) 1 0)))
              (build-list n x?))))
    (build-list n 1-at-x)))