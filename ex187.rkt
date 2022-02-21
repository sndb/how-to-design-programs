;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex187) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct gp [name score])
; A GamePlayer is a structure:
;    (make-gp String Number)
; interpretation: (make-gp p s) represents player p who
; scored a maximum of s points

(define a (make-gp "a" 3))
(define b (make-gp "b" 2))
(define c (make-gp "c" 1))

; A List-of-gameplayers is one of:
; — '()
; — (cons GamePlayer List-of-gameplayers)

; List-of-gameplayers -> List-of-gameplayers
; sorts l by scores in descending order

(check-expect (sort> '()) '())
(check-expect (sort> (list a b c))
              (list a b c))
(check-expect (sort> (list c b a))
              (list a b c))
(check-expect (sort> (list a c b))
              (list a b c))

(define (sort> l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort> (rest l)))]))

; GamePlayer List-of-gameplayers -> List-of-gameplayers
; inserts p into the sorted list l

(check-expect (insert a '())
              (list a))
(check-expect (insert a (list b c))
              (list a b c))
(check-expect (insert c (list a b))
              (list a b c))
(check-expect (insert b (list a c))
              (list a b c))

(define (insert p l)
  (cond
    [(empty? l) (list p)]
    [(cons? l)
     (if (gp>=? p (first l))
         (cons p l)
         (cons (first l) (insert p (rest l))))]))

; GamePlayer GamePlayer -> Boolean
; compares the scores of p1 and p2 with >=

(check-expect (gp>=? a b) #true)
(check-expect (gp>=? b c) #true)
(check-expect (gp>=? c a) #false)

(define (gp>=? p1 p2)
  (>= (gp-score p1) (gp-score p2)))