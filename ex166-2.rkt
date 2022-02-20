;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex166-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct employee [name number])
; An Employee is a structure:
;   (make-employee String Number)
; interpretation: (make-employee n i) combines the name n
; with the number i

(define-struct work [employee rate hours])
; A (piece of) Work is a structure:
;   (make-work Employee Number Number)
; interpretation: (make-work e r h) combines the employee e
; with the pay rate r and the number of hours h

; Low (short for list of works) is one of:
; – '()
; – (cons Work Low)
; interpretation: an instance of Low represents the
; hours worked for a number of employees

'()
(cons (make-work (make-employee "Robby" 1) 11.95 39)
      '())
(cons (make-work (make-employee "Matthew" 2) 12.95 45)
      (cons (make-work (make-employee "Robby" 1) 11.95 39)
            '()))

(define-struct paycheck [employee amount])
; A Paycheck is a structure:
;   (make-paycheck Employee Number)
; interpretation: (make-paycheck n a) combines the employee e
; with the amount a

; Lop (short for list of paychecks) is one of:
; — '()
; — (cons Paycheck Lop)
; interpretation: an instance of Lop represents the
; amounts earned for a number of employees

'()
(cons (make-paycheck (make-employee "Robby" 1) 466.05)
      '())
(cons (make-paycheck (make-employee "Matthew" 2) 582.75)
      (cons (make-paycheck (make-employee "Robby" 1) 466.05)
            '()))

; Low -> Lop
; computes the list of paychecks from an-lop

(check-expect
 (wage*.v4 (cons (make-work (make-employee "Robby" 1) 11.95 39) '()))
 (cons (make-paycheck (make-employee "Robby" 1) (* 11.95 39)) '()))

(define (wage*.v4 an-lop)
  (cond
    [(empty? an-lop) '()]
    [(cons? an-lop)
     (cons (wage.v4 (first an-lop))
           (wage*.v4 (rest an-lop)))]))

; Work -> Paycheck
; computes the paycheck for the given work record w
(define (wage.v4 w)
  (make-paycheck (work-employee w)
                 (* (work-rate w) (work-hours w))))