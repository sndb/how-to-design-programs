;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex428) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] N -> [List-of Number]
; produces a sorted version of alon
; uses sort< if the length of l is below th
; assume the numbers are all distinct

(check-expect (quick-sort< '() 5)
              '())
(check-expect (quick-sort< '(11) 5)
              '(11))
(check-expect (quick-sort< '(11 8 14 7) 5)
              '(7 8 11 14))
(check-expect (quick-sort< '(11 8 8 14 7) 3)
              '(7 8 8 11 14))
(check-expect (quick-sort< '(11 8 8 14 7) 5)
              '(7 8 8 11 14))
(check-expect (quick-sort< '(11 9 2 18 12 14 4 1) 5)
              '(1 2 4 9 11 12 14 18))

(define (quick-sort< alon th)
  (cond
    [(empty? alon) '()]
    [(= (length alon) 1) alon]
    [(< (length alon) th) (sort< alon)]
    [else (local ((define pivot (first alon)))
            (append (quick-sort< (smallers (rest alon) pivot) th)
                    (list pivot)
                    (quick-sort< (largers (rest alon) pivot) th)))]))
 
; [List-of Number] Number -> [List-of Number]
; filter out those items that are larger or equal to n
(define (largers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (>= (first alon) n)
              (cons (first alon) (largers (rest alon) n))
              (largers (rest alon) n))]))
 
; [List-of Number] Number -> [List-of Number]
; filter out those items that are smaller than n
(define (smallers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (< (first alon) n)
              (cons (first alon) (smallers (rest alon) n))
              (smallers (rest alon) n))]))

; [List-of Number] -> [List-of Number]
; produces a sorted version of l
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