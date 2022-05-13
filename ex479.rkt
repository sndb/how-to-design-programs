;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex479) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define QUEENS 8)
; A QP is a structure:
;   (make-posn CI CI)
; A CI is an N in [0,QUEENS).
; interpretation (make-posn r c) denotes the square at 
; the r-th row and c-th column

(define ex-q (make-posn 5 1))

; QP QP -> Boolean
; determines whether the queens threaten each other

(check-expect (threatening? ex-q (make-posn 5 7)) #true)
(check-expect (threatening? ex-q (make-posn 7 1)) #true)
(check-expect (threatening? ex-q (make-posn 7 3)) #true)
(check-expect (threatening? ex-q (make-posn 0 6)) #true)
(check-expect (threatening? ex-q (make-posn 4 0)) #true)
(check-expect (threatening? ex-q (make-posn 6 0)) #true)
(check-expect (threatening? ex-q (make-posn 3 7)) #false)
(check-expect (threatening? ex-q (make-posn 3 2)) #false)
(check-expect (threatening? ex-q (make-posn 0 7)) #false)
(check-expect (threatening? ex-q (make-posn 6 3)) #false)

(define (threatening? q1 q2)
  (or (= (posn-x q1) (posn-x q2))
      (= (posn-y q1) (posn-y q2))
      (= (+ (posn-x q1) (posn-y q1))
         (+ (posn-x q2) (posn-y q2)))
      (= (- (posn-x q1) (posn-y q1))
         (- (posn-x q2) (posn-y q2)))))