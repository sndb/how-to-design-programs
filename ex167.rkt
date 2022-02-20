;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex167) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-posns is one of:
; — '()
; — (cons Posn List-of-posns)

; List-of-posns -> Number
; produces the sum of all x-coordinates in l

(check-expect
 (sum '())
 0)
(check-expect
 (sum (cons (make-posn 3 4) '()))
 3)
(check-expect
 (sum (cons (make-posn 3 4)
            (cons (make-posn 6 8)
                  '())))
 9)

(define (sum l)
  (cond
    [(empty? l) 0]
    [(cons? l) (+ (posn-x (first l))
                  (sum (rest l)))]))