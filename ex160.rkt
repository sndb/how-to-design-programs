;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex160) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Son.L is one of:
; – empty
; – (cons Number Son.L)


; A Son.R is one of:
; – empty
; – (cons Number Son.R)
; Constraint: if s is a Son.R, no number occurs twice in s


; Son is used when it applies to Son.L and Son.R


; Son
(define es '())


; Number Son -> Boolean
; is x in s
(define (in? x s)
  (member? x s))


; Number Son.L -> Son.L
; removes x from s 
(define s1.L
  (cons 1 (cons 1 '())))

(check-expect
 (set-.L 1 s1.L) es)

(define (set-.L x s)
  (remove-all x s))


; Number Son.R -> Son.R
; removes x from s
(define s1.R
  (cons 1 '()))

(check-expect
 (set-.R 1 s1.R) es)

(define (set-.R x s)
  (remove x s))


; Number Son.L -> Son.L
; adds x to s

(check-expect
 (set+.L 1 (cons 1 (cons 1 '())))
 (cons 1 (cons 1 (cons 1 '()))))

(define (set+.L x s)
  (cons x s))


; Number Son.R -> Son.R
; adds x to s

(check-expect
 (set+.R 1 '())
 (cons 1 '()))
(check-expect
 (set+.R 1 (cons 1 '()))
 (cons 1 '()))

(define (set+.R x s)
  (if (not (in? x s))
      (cons x s)
      s))