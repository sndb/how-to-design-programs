;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex480) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define SIDE 24)
(define SQUARE (square SIDE 'outline 'black))
(define QUEEN (text "Q" 18 'black))

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

; N [List-of QP] Image -> Image
; produces an image of an n by n chess board with img placed according
; to l

(check-expect (render-queens 0 '() QUEEN) empty-image)
(check-expect
 (render-queens 1 `(,(make-posn 0 0)) QUEEN)
 (place-image QUEEN (/ SIDE 2) (/ SIDE 2) (render-board 1)))
(check-expect
 (render-queens 2 `(,(make-posn 0 0) ,(make-posn 1 1)) QUEEN)
 (place-image QUEEN
              (/ SIDE 2) (/ SIDE 2)
              (place-image QUEEN
                           (+ SIDE (/ SIDE 2)) (+ SIDE (/ SIDE 2))
                           (render-board 2))))

(define (render-queens n l img)
  (foldr (lambda (q i)
           (place-image img
                        (+ (/ SIDE 2) (* SIDE (posn-x q)))
                        (+ (/ SIDE 2) (* SIDE (posn-y q)))
                        i))
         (render-board n)
         l))

; N -> Image
; produces an image of an n by n chess board

(check-expect (render-board 0) empty-image)
(check-expect (render-board 1) SQUARE)
(check-expect (render-board 2)
              (above (beside SQUARE SQUARE)
                     (beside SQUARE SQUARE)))

(define (render-board n)
  (local (; N -> Image
          (define (render-row k)
            (cond
              [(zero? k) empty-image]
              [else (beside SQUARE (render-row (sub1 k)))]))

          ; N -> Image
          (define (render-full k)
            (cond
              [(zero? k) empty-image]
              [else (above row (render-full (sub1 k)))]))

          ; Image
          (define row (render-row n)))
    (render-full n)))