;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex360) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ERROR-CON-DEF "cannot find a constant definition")
(define ERROR-FUN-DEF "cannot find a function definition")

(define-struct fun [name arg])
(define-struct add [left right])
(define-struct mul [left right])
; A BSL-expr is one of: 
; – Number
; – Symbol
; — (make-fun Symbol BSL-expr)
; – (make-add BSL-expr BSL-expr)
; – (make-mul BSL-expr BSL-expr)

(define-struct def [name param body])
(define-struct con [name value])
; A BSL-fun-def is a structure:
;   (make-def Symbol Symbol BSL-expr)
; A BSL-con-def is a structure:
;   (make-con Symbol BSL-expr)
; A BSL-def is one of:
; — BSL-fun-def
; — BSL-con-def
; Examples:
(define close-to-pi
  (make-con 'close-to-pi 3.14))
(define area-of-circle
  (make-def 'area-of-circle 'r
            (make-mul 'close-to-pi (make-mul 'r 'r))))
(define volume-of-10-cylinder
  (make-def 'volume-of-10-cylinder 'r
            (make-mul 10 (make-fun 'area-of-circle 'r))))

; A BSL-da-all is a [List-of BSL-def].
; Examples:
(define ex-da
  (list close-to-pi area-of-circle volume-of-10-cylinder))

; BSL-da-all Symbol -> BSL-con-def
; finds a constant definition of x; error otherwise

(check-expect (lookup-con-def ex-da 'close-to-pi)
              close-to-pi)
(check-error (lookup-con-def ex-da 'area-of-circle)
             ERROR-CON-DEF)
(check-error (lookup-con-def ex-da 'close-to-tau)
             ERROR-CON-DEF)

(define (lookup-con-def da x)
  (cond
    [(empty? da) (error ERROR-CON-DEF)]
    [(cons? da)
     (if (and (con? (first da))
              (symbol=? x (con-name (first da))))
         (first da)
         (lookup-con-def (rest da) x))]))

; BSL-da-all Symbol -> BSL-fun-def
; finds a function definition of f; error otherwise

(check-expect (lookup-fun-def ex-da 'area-of-circle)
              area-of-circle)
(check-expect (lookup-fun-def ex-da 'volume-of-10-cylinder)
              volume-of-10-cylinder)
(check-error (lookup-fun-def ex-da 'close-to-pi)
             ERROR-FUN-DEF)
(check-error (lookup-fun-def ex-da 'area-of-square)
             ERROR-FUN-DEF)

(define (lookup-fun-def da f)
  (cond
    [(empty? da) (error ERROR-FUN-DEF)]
    [(cons? da)
     (if (and (def? (first da))
              (symbol=? f (def-name (first da))))
         (first da)
         (lookup-fun-def (rest da) f))]))