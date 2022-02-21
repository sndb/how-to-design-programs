;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex189) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number List-of-numbers -> Boolean
(define (search n alon)
  (cond
    [(empty? alon) #false]
    [else (or (= (first alon) n)
              (search n (rest alon)))]))

; Number List-of-numbers -> Boolean
; determines whether n occurs in the sorted list alon

(check-expect (search-sorted 1 '())
              #false)
(check-expect (search-sorted 1 (list 12 1 -5))
              #true)
(check-expect (search-sorted 2 (list 12 1 -5))
              #false)
(check-expect (search-sorted -5 (list 12 1 -5))
              #true)
(check-expect (search-sorted 13 (list 12 1 -5))
              #false)

(define (search-sorted n alon)
  (cond
    [(empty? alon) #false]
    [(cons? alon)
     (or (= n (first alon))
         (and (< n (first alon))
              (search-sorted n (rest alon))))]))