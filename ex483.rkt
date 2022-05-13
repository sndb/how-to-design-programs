;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex483) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define-struct board (size queens))
; A Board is a structure:
;   (make-board N [List-of QP])

; data example: QP
(define EX-QP (make-posn 5 1))

; data example: [List-of QP]
(define 4QUEEN-SOLUTION-1
  (list (make-posn 0 1) (make-posn 1 3)
        (make-posn 2 0) (make-posn 3 2)))
(define 4QUEEN-SOLUTION-2
  (list (make-posn 0 2) (make-posn 1 0)
        (make-posn 2 3) (make-posn 3 1)))

; QP QP -> Boolean
; determines whether the queens threaten each other

(check-expect (threatening? EX-QP (make-posn 5 7)) #true)
(check-expect (threatening? EX-QP (make-posn 7 1)) #true)
(check-expect (threatening? EX-QP (make-posn 7 3)) #true)
(check-expect (threatening? EX-QP (make-posn 0 6)) #true)
(check-expect (threatening? EX-QP (make-posn 4 0)) #true)
(check-expect (threatening? EX-QP (make-posn 6 0)) #true)
(check-expect (threatening? EX-QP (make-posn 3 7)) #false)
(check-expect (threatening? EX-QP (make-posn 3 2)) #false)
(check-expect (threatening? EX-QP (make-posn 0 7)) #false)
(check-expect (threatening? EX-QP (make-posn 6 3)) #false)

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

; N -> [Maybe [List-of QP]]
; finds a solution to the n queens problem 

(check-expect (n-queens 0) '())
(check-satisfied (n-queens 1) (n-queens-solution? 1))
(check-expect (n-queens 2) #false)
(check-expect (n-queens 3) #false)
(check-satisfied (n-queens 4) (n-queens-solution? 4))
(check-satisfied (n-queens 5) (n-queens-solution? 5))
(check-satisfied (n-queens 6) (n-queens-solution? 6))
(check-satisfied (n-queens 7) (n-queens-solution? 7))
(check-satisfied (n-queens 8) (n-queens-solution? 8))
 
(define (n-queens n)
  (place-queens (board0 n) n))

; N -> [[List-of QP] -> Boolean]
; produces a predicate on queen placements that determines whether a
; given placement is a solution to an n queens puzzle

(check-expect ((n-queens-solution? 4) 4QUEEN-SOLUTION-1) #true)
(check-expect ((n-queens-solution? 4) 4QUEEN-SOLUTION-2) #true)
(check-expect ((n-queens-solution? 5) 4QUEEN-SOLUTION-2) #false)
(check-expect ((n-queens-solution? 3) 4QUEEN-SOLUTION-2) #false)
(check-expect ((n-queens-solution? 2)
               (list (make-posn 0 1)
                     (make-posn 1 0)))
              #false)
(check-expect ((n-queens-solution? 4)
               (list (make-posn 0 2) (make-posn 1 0)
                     (make-posn 2 3) (make-posn 3 0)))
              #false)

(define (n-queens-solution? n)
  (lambda (solution)
    (and (list? solution)
         (= n (length solution))
         (andmap posn? solution)
         (andmap (lambda (p)
                   (andmap (lambda (q)
                             (not (threatening? q p)))
                           (remove p solution)))
                 solution))))

; Board N -> [Maybe [List-of QP]]
; places n queens on board; otherwise, returns #false
(define (place-queens a-board n)
  (cond
    [(zero? n) (board-queens a-board)]
    [else
     (foldr (lambda (qp solution)
              (if (boolean? solution)
                  (place-queens (add-queen a-board qp) (sub1 n))
                  solution))
            #false
            (find-open-spots a-board))]))

; N -> Board 
; creates the initial n by n board

(check-expect (board0 4) (make-board 4 '()))

(define (board0 n)
  (make-board n '()))

; Board QP -> Board 
; places a queen at qp on a-board

(check-expect (add-queen (board0 3) (make-posn 1 1))
              (make-board 3 (list (make-posn 1 1))))

(define (add-queen a-board qp)
  (make-board (board-size a-board)
              (cons qp (board-queens a-board))))
 
; Board -> [List-of QP]
; finds spots where it is still safe to place a queen

(check-expect
 (find-open-spots (make-board 3 (list (make-posn 1 1))))
 '())
(check-expect
 (find-open-spots (make-board 3 (list (make-posn 0 0))))
 (list (make-posn 2 1) (make-posn 1 2)))

(define (find-open-spots a-board)
  (local ((define size (board-size a-board))
          (define queens (board-queens a-board))
          (define potential-spots
            (build-list (* size size)
                        (lambda (n)
                          (make-posn (remainder n size)
                                     (quotient n size))))))
    (filter (lambda (p)
              (andmap (lambda (q) (not (threatening? p q)))
                      queens))
            potential-spots)))