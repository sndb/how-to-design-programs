;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex162) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PER-HOUR 14)
(define EXCEED-HOURS 100)

; A List-of-numbers is one of:
; – '()
; – (cons Number List-of-numbers)

; List-of-numbers -> List-of-numbers
; computes the weekly wages for all given weekly hours

(check-expect (wage* '()) '())
(check-expect (wage* (cons 28 '()))
              (cons (* PER-HOUR 28) '()))
(check-expect (wage* (cons 4 (cons 2 '())))
              (cons (* PER-HOUR 4) (cons (* PER-HOUR 2) '())))
(check-expect (wage* (cons EXCEED-HOURS '()))
              (cons (* PER-HOUR EXCEED-HOURS) '()))
(check-error (wage* (cons (+ EXCEED-HOURS 1) '())))

(define (wage* whrs)
  (cond
    [(empty? whrs) '()]
    [else
     (if (> (first whrs) EXCEED-HOURS)
         (error "wage*: item "
                (first whrs)
                " of the input list exceeds "
                EXCEED-HOURS)
         (cons (wage (first whrs)) (wage* (rest whrs))))]))

; Number -> Number
; computes the wage for h hours of work
(define (wage h)
  (* PER-HOUR h))