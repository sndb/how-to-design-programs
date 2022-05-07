;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex441) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of X] [X X -> Boolean] -> [List-of X]
; produces a sorted version of alon with items compared with cmp
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

; [List-of Number] -> [List-of Number]
; sorts alon
(define (quick-sort< alon)
  (quick-sort alon <))

(quick-sort< (list 10 6 8 9 14 12 3 11 14 16 2))

(append (quick-sort< '(6 8 9 3 2))
        '(10)
        (quick-sort< '(14 12 11 14 16)))

(append (append (quick-sort< '(3 2))
                '(6)
                (quick-sort< '(8 9)))
        '(10)
        (append (quick-sort< '(12 11))
                '(14)
                (quick-sort< '(14 16))))

(append (append (append (quick-sort< '(2)) '(3) (quick-sort< '()))
                '(6)
                (append (quick-sort< '()) '(8) (quick-sort< '(9))))
        '(10)
        (append (append (quick-sort< '(11)) '(12) (quick-sort< '()))
                '(14)
                (append (quick-sort< '()) '(14) (quick-sort< '(16)))))

(append (append (append '(2) '(3))
                '(6)
                (append '(8) '(9)))
        '(10)
        (append (append '(11) '(12))
                '(14)
                (append '(14) '(16))))

(append (append '(2 3)
                '(6)
                '(8 9))
        '(10)
        (append '(11 12)
                '(14)
                '(14 16)))

(append '(2 3 6 8 9)
        '(10)
        '(11 12 14 14 16))

'(2 3 6 8 9 10 11 12 14 14 16)

; 15 recursive applications of quick-sort< are required.
; 7 recursive applications of append are required.

(quick-sort< (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14))

(append (quick-sort< '()) '(1)
        (append (quick-sort< '()) '(2)
                (append (quick-sort< '()) '(3)
                        (append (quick-sort< '()) '(4)
                                (append (quick-sort< '()) '(5)
                                        (append (quick-sort< '()) '(6)
                                                (append (quick-sort< '()) '(7)
                                                        (append (quick-sort< '()) '(8)
                                                                (append (quick-sort< '()) '(9)
                                                                        (append (quick-sort< '()) '(10)
                                                                                (append (quick-sort< '()) '(11)
                                                                                        (append (quick-sort< '()) '(12)
                                                                                                (append (quick-sort< '()) '(13)
                                                                                                        (append (quick-sort< '()) '(14)
                                                                                                                (quick-sort< '())))))))))))))))

; 15 recursive applications of quick-sort< are required.
; 14 recursive applications of append are required.