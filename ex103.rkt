;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex103) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct space [x y z])
; A Space is a structure:
;   (make-space [x y z])

(define-struct spider [legs space])
; A Spider is a structure:
;   (make-spider Number Space)
; interpretation: represents the spider that has n remaining
; legs and needs s space in case of transport

; An Elephant is a Space.
; interpretation: represents the space the elephant needs in case of transport

(define-struct boa-constrictor [length girth])
; A BoaConstrictor is a structure:
;   (make-boa-constrictor Number Number)
; interpretation: (make-boa-constrictor l g) describes the boa constrictor
; whose length is l and girth is g

; An Armadillo is a Number.
; interpretation: represents the radius of the armadillo

; A ZooAnimal is one of:
; — Spider
; — Elephant
; — BoaConstrictor
; — Armadillo

; ZooAnimal Space -> Boolean
; determines whether c is large enough for a
(check-expect (fits? (make-spider 4 (make-space 2 8 4)) (make-space 3 10 5)) #true)
(check-expect (fits? (make-spider 4 (make-space 2 8 4)) (make-space 2 8 5)) #true)
(check-expect (fits? (make-spider 4 (make-space 2 8 4)) (make-space 2 8 4)) #false)
(check-expect (fits? (make-space 100 20 30) (make-space 100 40 40)) #true)
(check-expect (fits? (make-space 100 20 30) (make-space 100 20 20)) #false)
(check-expect (fits? (make-boa-constrictor 20 3) (make-space 10 10 10)) #true)
(check-expect (fits? (make-boa-constrictor 20 3) (make-space 1 1 1)) #false)
(check-expect (fits? 1 (make-space 2 2 2)) #true)
(check-expect (fits? 1 (make-space 1 1 1)) #false)
(define (fits? a c)
  (cond
    [(spider? a) (> (space->volume c) (space->volume (spider-space a)))]
    [(space? a) (> (space->volume c) (space->volume a))]
    [(boa-constrictor? a) (> (space->volume c)
                             (* (boa-constrictor-length a)
                                (circumference->area (boa-constrictor-girth a))))]
    [(number? a) (> (space->volume c) (radius->volume a))]))

; Space -> Number
; computes the volume of s
(check-expect (space->volume (make-space 5 5 5)) 125)
(check-expect (space->volume (make-space 1 2 3)) 6)
(check-expect (space->volume (make-space 0 8 2)) 0)
(define (space->volume s)
  (* (space-x s) (space-y s) (space-z s)))

; Number -> Number
; computes the area of a circle from its circumference c
(check-within (circumference->area 17) 23 0.01)
(define (circumference->area c)
  (/ (sqr c) (* pi 4)))

; Number -> Number
; computes the volume of a sphere from its radius r
(check-within (radius->volume 1) 4.2 0.1)
(define (radius->volume r)
  (* 4/3 pi (expt r 3)))