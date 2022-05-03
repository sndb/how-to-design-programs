;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex430) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of X] [X X -> Boolean] -> [List-of X]
; produces a sorted version of alon with items compared with cmp

(check-expect (quick-sort '() <)
              '())
(check-expect (quick-sort '(11) <)
              '(11))
(check-expect (quick-sort '(11 8 14 7) <)
              '(7 8 11 14))
(check-expect (quick-sort '(11 8 8 14 7) <)
              '(7 8 8 11 14))
(check-expect (quick-sort '(11 9 2 18 12 14 4 1) <)
              '(1 2 4 9 11 12 14 18))
(check-expect (quick-sort '(11 9 2 18 12 14 4 1) >)
              '(18 14 12 11 9 4 2 1))

(define (quick-sort alon cmp)
  (cond
    [(or (empty? alon) (empty? (rest alon))) alon]
    [else (local ((define pivot (first alon))
                  (define smallers
                    (filter (lambda (x) (cmp x pivot))
                            (rest alon)))
                  (define largers
                    (filter (lambda (x) (not (cmp x pivot)))
                            (rest alon))))
            (append (quick-sort smallers cmp)
                    (list pivot)
                    (quick-sort largers cmp)))]))