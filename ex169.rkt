;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex169) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-posns is one of:
; — '()
; — (cons Posn List-of-posns)

; List-of-posns -> List-of-posns
; produces a list of Posns whose x-coordinates are between 0 and 100
; and whose y-coordinates are between 0 and 200

(check-expect
 (legal (cons (make-posn 0 0) '()))
 (cons (make-posn 0 0) '()))
(check-expect
 (legal (cons (make-posn 100 200) '()))
 (cons (make-posn 100 200) '()))
(check-expect
 (legal (cons (make-posn 101 200) '()))
 '())
(check-expect
 (legal (cons (make-posn 100 201) '()))
 '())

(define (legal l)
  (cond
    [(empty? l) '()]
    [(cons? l) (if (valid? (first l))
                   (cons (first l) (legal (rest l)))
                   (legal (rest l)))]))

; Posn -> Boolean
; is x-coordinate of p is between 0 and 100,
; and y-coordinate of p is between 0 and 200
(define (valid? p)
  (and (<= 0 (posn-x p) 100)
       (<= 0 (posn-y p) 200)))