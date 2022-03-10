;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex240) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct layer [stuff])

; An LStr is one of: 
; – String
; – (make-layer LStr)

"abc"
(make-layer (make-layer "xyz"))
    
; An LNum is one of: 
; – Number
; – (make-layer LNum)

1
(make-layer (make-layer 2))

; An [Layer X] is one of:
; — X
; — (make-layer [Layer X])

; An LStr is a [Layer String].
; An LNum is a [Layer Number].