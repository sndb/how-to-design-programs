;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex147) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An NEList-of-booleans is one of:
; — (cons Boolean '())
; — (cons Boolean NEList-of-booleans)
; interpretation: non-empty lists of boolean values

(cons #false '())
(cons #false (cons #true '()))
(cons #true (cons #true (cons #true '())))

; NEList-of-booleans -> Boolean
; determines whether all items of l are #true

(check-expect (all-true (cons #false '())) #false)
(check-expect (all-true (cons #false (cons #true '()))) #false)
(check-expect (all-true (cons #true (cons #true (cons #true '())))) #true)

(define (all-true l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (and (first l) (all-true (rest l)))]))

; NEList-of-booleans -> Boolean
; determines whether at least one item of l is #true

(check-expect (one-true (cons #false '())) #false)
(check-expect (one-true (cons #false (cons #true '()))) #true)
(check-expect (one-true (cons #true (cons #true (cons #true '())))) #true)

(define (one-true l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (or (first l) (one-true (rest l)))]))