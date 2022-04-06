;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex331) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Dir.v1 (short for directory) is one of: 
; – '()
; – (cons File.v1 Dir.v1)
; – (cons Dir.v1 Dir.v1)
 
; A File.v1 is a String.

(define figure123
  '(("part1" "part2" "part3")
    "read!"
    (("hang" "draw")
     ("read!"))))

; Any -> Boolean
; is f a file
(define (file? f) (string? f))

; Dir.v1 -> N
; determines how many files dir contains

(check-expect (how-many '()) 0)
(check-expect (how-many figure123) 7)

(define (how-many dir)
  (cond
    [(empty? dir) 0]
    [(file? (first dir))
     (+ 1 (how-many (rest dir)))]
    [else
     (+ (how-many (first dir)) (how-many (rest dir)))]))