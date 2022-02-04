;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex79) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Color is one of: 
; — "white"
; — "yellow"
; — "orange"
; — "green"
; — "red"
; — "blue"
; — "black"

"white"
"green"
"blue"

; H is a Number between 0 and 100.
; interpretation: represents a happiness value

0
42
74
100

(define-struct person [fstname lstname male?])
; A Person is a structure:
;   (make-person String String Boolean)

(make-person "John" "Doe" #true)
(make-person "Elizabeth" "Cowell" #false)
(make-person "Richard" "Stallman" #true)
(make-person "Alan" "Turing" #true)

(define-struct dog [owner name age happiness])
; A Dog is a structure:
;   (make-dog Person String PositiveInteger H)
; interpretation: represents a dog with an owner,
; a name, an age, and a happiness level

(make-dog (make-person "John" "Doe" #true)
          "Jack"
          7
          90)

; A Weapon is one of:
; — #false
; — Posn
; interpretation #false means the missile hasn't
; been fired yet; a Posn means it is in flight

#false
(make-posn 12 42)
(make-posn 23 14)