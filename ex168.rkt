;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex168) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-posns is one of:
; — '()
; — (cons Posn List-of-posns)

; List-of-posns -> List-of-posns
; increment the y-coordinate of each item in l by 1

(check-expect (translate '()) '())
(check-expect
 (translate (cons (make-posn 3 4)
                  '()))
 (cons (make-posn 3 5)
       '()))
(check-expect
 (translate (cons (make-posn 3 4)
                  (cons (make-posn 4 8)
                        '())))
 (cons (make-posn 3 5)
       (cons (make-posn 4 9)
             '())))

(define (translate l)
  (cond
    [(empty? l) '()]
    [(cons? l)
     (cons (y+1 (first l)) (translate (rest l)))]))

; Posn -> Posn
; increment the y-coordinate of p by 1
(define (y+1 p)
  (make-posn (posn-x p) (add1 (posn-y p))))