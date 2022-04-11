;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex358) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct fun [name arg])
(define-struct add [left right])
(define-struct mul [left right])

(define-struct def [name parameter body])
; BSL-fun-def is a structure:
;   (make-def Symbol Symbol BSL-fun-expr)
; Examples:
(define (f x) (+ 3 x))
(define fd-f
  (make-def 'f 'x (make-add 3 'x)))
(define (g y) (f (* 2 y)))
(define fd-g
  (make-def 'g 'y (make-fun 'f (make-mul 2 'y))))
(define (h v) (+ (f v) (g v)))
(define fd-h
  (make-def 'h 'v (make-add (make-fun 'f 'v) (make-fun 'g 'v))))

; BSL-fun-def* is a [List-of BSL-fun-def].
; Example:
(define da-fgh (list fd-f fd-g fd-h))

; BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of f in da
; signals an error if there is none

(check-expect (lookup-def da-fgh 'g) fd-g)
(check-error (lookup-def da-fgh 'a)
             (string-append DEFINITION-NOT-FOUND "a"))

(define (lookup-def da f)
  (cond
    [(empty? da) (error DEFINITION-NOT-FOUND (symbol->string f))]
    [(cons? da)
     (if (symbol=? f (def-name (first da)))
         (first da)
         (lookup-def (rest da) f))]))

(define DEFINITION-NOT-FOUND "lookup-def: definition not found: ")