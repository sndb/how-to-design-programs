;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex442) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define CROSSOVER-POINT 50)

; [List-of Number] -> [List-of Number]
; sorts alon using quick-sort< for large lists and sort< for short lists

(check-satisfied (clever-sort (create-tests 10))
                 (lambda (l) (sorted? <= l)))
(check-satisfied (clever-sort (create-tests 1000))
                 (lambda (l) (sorted? <= l)))

(define (clever-sort alon)
  (if (< (length alon) CROSSOVER-POINT)
      (sort< alon)
      (quick-sort< alon)))

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

(check-expect (quick-sort< '()) '())
(check-expect (quick-sort< '(11)) '(11))
(check-expect (quick-sort< '(11 8 14 7)) '(7 8 11 14))
(check-expect (quick-sort< '(11 8 8 14 7)) '(7 8 8 11 14))
(check-expect (quick-sort< '(11 9 2 18 12 14 4 1))
              '(1 2 4 9 11 12 14 18))

(define (quick-sort< alon)
  (quick-sort alon <))

; [List-of Number] -> [List-of Number]
; produces a sorted version of l

(check-expect (sort< '()) '())
(check-expect (sort< '(11)) '(11))
(check-expect (sort< '(11 8 14 7)) '(7 8 11 14))
(check-expect (sort< '(11 8 8 14 7)) '(7 8 8 11 14))
(check-expect (sort< '(11 9 2 18 12 14 4 1)) '(1 2 4 9 11 12 14 18))

(define (sort< l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort< (rest l)))]))

; Number [List-of Number] -> [List-of Number]
; inserts n into the sorted list of numbers l
(define (insert n l)
  (cond
    [(empty? l) (list n)]
    [else (if (<= n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))

; N -> [List-of Number]
; creates a list of random numbers of length n

(check-satisfied (create-tests 10000)
                 (lambda (l)
                   (not (or (sorted? <= l)
                            (sorted? >= l)))))

(define (create-tests n)
  (cond
    [(zero? n) '()]
    [else (cons (random 100) (create-tests (sub1 n)))]))

; [X X -> Boolean] [NEList-of X] -> Boolean 
; determines whether l is sorted according to cmp
 
(check-expect (sorted? < '(1 2 3)) #true)
(check-expect (sorted? < '(2 1 3)) #false)
 
(define (sorted? cmp l)
  (cond
    [(empty? (rest l)) #true]
    [else
     (and (cmp (first l) (second l))
          (sorted? cmp (rest l)))]))

; short lists
;> (time (sort< (create-tests 20)))
;cpu time: 0 real time: 0 gc time: 0
;> (time (quick-sort< (create-tests 20)))
;cpu time: 1 real time: 1 gc time: 0

; long lists
;> (time (sort< (create-tests 10000)))
;cpu time: 15614 real time: 15645 gc time: 1152
;> (time (quick-sort< (create-tests 10000)))
;cpu time: 201 real time: 202 gc time: 28