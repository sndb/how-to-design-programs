;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex188) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct email [from date message])
; An EmailMessage is a structure:
;   (make-email String Number String)
; interpretation: (make-email f d m) represents text m
; sent by f, d seconds after the beginning of time

(define a (make-email "a" 1 "hello"))
(define b (make-email "b" 2 "world"))
(define c (make-email "c" 3 "good bye"))

; A List-of-emails is one of:
; — '()
; — (cons EmailMessage List-of-emails)

; List-of-emails -> List-of-emails
; sorts the list of emails by date in descending order

(check-expect (sort-by-date> '()) '())
(check-expect (sort-by-date> (list a b c))
              (list c b a))
(check-expect (sort-by-date> (list c b a))
              (list c b a))
(check-expect (sort-by-date> (list a c b))
              (list c b a))

(define (sort-by-date> l)
  (cond
    [(empty? l) '()]
    [(cons? l)
     (insert-by-date> (first l) (sort-by-date> (rest l)))]))

; EmailMessage List-of-emails -> List-of-emails
; inserts e in the sorted (by date in descending order) list l

(check-expect (insert-by-date> a '())
              (list a))
(check-expect (insert-by-date> a (list c b))
              (list c b a))
(check-expect (insert-by-date> c (list b a))
              (list c b a))
(check-expect (insert-by-date> b (list c a))
              (list c b a))

(define (insert-by-date> e l)
  (cond
    [(empty? l) (list e)]
    [(cons? l)
     (if (date>=? e (first l))
         (cons e l)
         (cons (first l) (insert-by-date> e (rest l))))]))

; EmailMessage EmailMessage -> Boolean
; compares e1 and e2 by date with >=

(check-expect (date>=? c b) #true)
(check-expect (date>=? a c) #false)

(define (date>=? e1 e2)
  (>= (email-date e1) (email-date e2)))

; List-of-emails -> List-of-emails
; sorts the list of emails by name in ascending order

(check-expect (sort-by-name< '()) '())
(check-expect (sort-by-name< (list a b c))
              (list a b c))
(check-expect (sort-by-name< (list c b a))
              (list a b c))
(check-expect (sort-by-name< (list a c b))
              (list a b c))

(define (sort-by-name< l)
  (cond
    [(empty? l) '()]
    [(cons? l)
     (insert-by-name< (first l) (sort-by-name< (rest l)))]))

; EmailMessage List-of-emails -> List-of-emails
; inserts e in the sorted (by name in ascending order) list l

(check-expect (insert-by-name< a '())
              (list a))
(check-expect (insert-by-name< a (list b c))
              (list a b c))
(check-expect (insert-by-name< c (list a b))
              (list a b c))
(check-expect (insert-by-name< b (list a c))
              (list a b c))

(define (insert-by-name< e l)
  (cond
    [(empty? l) (list e)]
    [(cons? l)
     (if (name<? e (first l))
         (cons e l)
         (cons (first l) (insert-by-name< e (rest l))))]))

; EmailMessage EmailMessage -> Boolean
; compares e1 and e2 by name with string<?

(check-expect (name<? c b) #false)
(check-expect (name<? a c) #true)

(define (name<? e1 e2)
  (string<? (email-from e1) (email-from e2)))