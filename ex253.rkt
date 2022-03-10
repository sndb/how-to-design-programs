;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex253) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [Number -> Boolean]
(check-expect (even? 2) #true)

; [Boolean String -> Boolean]
(check-expect (equal? #true "a") #false)

; [Number Number Number -> Number]
(check-expect (+ 1 2 3) 6)

; [Number -> [List-of Number]]
(check-expect (list 5) '(5))

; [[List-of Number] -> Boolean]
(check-expect (list? '(1 2 3)) #true)