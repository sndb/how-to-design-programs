;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex159) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define COLUMNS 10)
(define ROWS 20)
(define SIDE 10)
(define SQUARE (empty-scene SIDE SIDE))
(define SCENE-WIDTH (* COLUMNS SIDE))
(define SCENE-HEIGHT (* ROWS SIDE))
(define SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT))
(define DOT (circle 3 "solid" "red"))
(define TICK-RATE 1)

; An N is one of:
; – 0
; – (add1 N)
; interpretation: represents the counting numbers

; A List-of-posns is one of: 
; – '()
; – (cons Posn List-of-posns)

(define-struct pair [balloon# lob])
; A Pair is a structure (make-pair N List-of-posns)
; interpretation: (make-pair n lob) means n balloons 
; must yet be thrown and added to lob

; N -> List-of-posns
; consumes how many balloons the students want to throw
; shows one balloon dropping after another at TICK-RATE
; produces the list of Posns where the balloons hit
(define (riot n)
  (pair-lob
   (big-bang (make-pair n '())
     [to-draw render]
     [on-tick tock TICK-RATE])))

; Pair -> Image
; renders the thrown balloons

(check-expect (render (make-pair 0 '())) (add-balloons '()))
(check-expect (render (make-pair 42 '())) (add-balloons '()))
(check-expect
 (render (make-pair 4 (cons (make-posn 30 40) '())))
 (add-balloons (cons (make-posn 30 40) '())))
(check-expect
 (render (make-pair 0 (cons (make-posn 70 120)
                            (cons (make-posn 30 40)
                                  '()))))
 (add-balloons (cons (make-posn 70 120)
                     (cons (make-posn 30 40)
                           '()))))

(define (render p)
  (add-balloons (pair-lob p)))

; Pair -> Pair
; adds a new balloon to the list and decrements the counter

(check-expect (tock (make-pair 0 '())) (make-pair 0 '()))
(check-random (tock (make-pair 1 '()))
              (make-pair 0 (append-posn '())))
(check-random (tock (make-pair 1 (cons (make-posn 30 40) '())))
              (make-pair 0 (append-posn (cons (make-posn 30 40) '()))))

(define (tock p)
  (cond
    [(positive? (pair-balloon# p))
     (make-pair
      (sub1 (pair-balloon# p))
      (append-posn (pair-lob p)))]
    [else p]))

; List-of-posns -> List-of-posns
; appends a random Posn in the interval
; [(0,0),(SCENE-WIDTH,SCENE-HEIGHT)) to l

(check-random (append-posn '())
              (cons (make-posn (random SCENE-WIDTH)
                               (random SCENE-HEIGHT))
                    '()))
(check-random (append-posn (cons (make-posn 30 40) '()))
              (cons (make-posn (random SCENE-WIDTH)
                               (random SCENE-HEIGHT))
                    (cons (make-posn 30 40) '())))

(define (append-posn l)
  (cons (make-posn (random SCENE-WIDTH)
                   (random SCENE-HEIGHT))
        l))

; N Image -> Image
; produces a vertical arrangement of n copies of img

(check-expect
 (col 3 (square 20 "solid" "red"))
 (above (square 20 "solid" "red")
        (square 20 "solid" "red")
        (square 20 "solid" "red")))

(define (col n img)
  (cond
    [(zero? n) empty-image]
    [(positive? n) (above img (col (sub1 n) img))]))

; N Image -> Image
; produces a horizontal arrangement of n copies of img

(check-expect
 (row 3 (square 20 "solid" "red"))
 (beside (square 20 "solid" "red")
         (square 20 "solid" "red")
         (square 20 "solid" "red")))

(define (row n img)
  (cond
    [(zero? n) empty-image]
    [(positive? n) (beside img (row (sub1 n) img))]))

(define LECTURE-HALL
  (place-image (col ROWS (row COLUMNS SQUARE))
               (/ SCENE-WIDTH 2)
               (/ SCENE-HEIGHT 2)
               SCENE))

; List-of-posns -> Image
; produces an image of the lecture hall with red dots added
; as specified by the Posns

(check-expect
 (add-balloons (cons (make-posn 30 40) (cons (make-posn 70 120) '())))
 (place-dot (make-posn 30 40)
            (place-dot (make-posn 70 120)
                       LECTURE-HALL)))

(define (add-balloons l)
  (cond
    [(empty? l) LECTURE-HALL]
    [else (place-dot (first l) (add-balloons (rest l)))]))

; Posn Image -> Image
; places DOT at p on img

(check-expect
 (place-dot (make-posn 30 40) LECTURE-HALL)
 (place-image DOT 30 40 LECTURE-HALL))

(define (place-dot p img)
  (place-image DOT (posn-x p) (posn-y p) img))