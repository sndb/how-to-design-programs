;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex404) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct db [schema content])
; A DB is a structure: (make-db Schema Content)
 
; A Schema is a [List-of Spec]
; A Spec is a [List Label Predicate]
; A Label is a String
; A Predicate is a [Any -> Boolean]
 
; A (piece of) Content is a [List-of Row]
; A Row is a [List-of Cell]
; A Cell is Any
; constraint cells do not contain functions 
 
; integrity constraint In (make-db sch con), 
; for every row in con,
; (I1) its length is the same as sch's, and
; (I2) its ith Cell satisfies the ith Predicate in sch

(define school-schema
  `(("Name"    ,string?)
    ("Age"     ,integer?)
    ("Present" ,boolean?)))

(define school-content
  `(("Alice" 35 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)))

(define school-db
  (make-db school-schema
           school-content))

(define presence-schema
  `(("Present"     ,boolean?)
    ("Description" ,string?)))

(define presence-content
  `((#true  "presence")
    (#false "absence")))

(define presence-db
  (make-db presence-schema
           presence-content))

; [X Y] [X Y -> Boolean] [List-of X] [List-of Y] -> Boolean
; applies f to pairs of corresponding values from l1 and l2
; #true if f always produces #true; #false otherwise
; assume: input lists are of same length

(check-expect (andmap2 string=?
                       '("hello" "world")
                       '("hello" "world"))
              #true)

(check-expect (andmap2 string=?
                       '("hello" "world")
                       '("hi" "world"))
              #false)

(define (andmap2 f l1 l2)
  (cond
    [(empty? l1) #true]
    [(cons? l1)
     (and (f (first l1) (first l2))
          (andmap2 f (rest l1) (rest l2)))]))