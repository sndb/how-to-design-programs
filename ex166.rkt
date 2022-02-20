;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex166) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct work [employee rate hours])
; A (piece of) Work is a structure:
;   (make-work String Number Number)
; interpretation: (make-work n r h) combines the name
; with the pay rate r and the number of hours h

; Low (short for list of works) is one of:
; – '()
; – (cons Work Low)
; interpretation: an instance of Low represents the
; hours worked for a number of employees

'()
(cons (make-work "Robby" 11.95 39)
      '())
(cons (make-work "Matthew" 12.95 45)
      (cons (make-work "Robby" 11.95 39)
            '()))
(cons (make-work "John" 17.45 30)
      (cons (make-work "Matthew" 12.95 45)
            (cons (make-work "Robby" 11.95 39)
                  '())))
(cons (make-work "Bill" 9.95 42)
      (cons (make-work "John" 17.45 30)
            (cons (make-work "Matthew" 12.95 45)
                  (cons (make-work "Robby" 11.95 39)
                        '()))))

(define-struct paycheck [employee amount])
; A Paycheck is a structure:
;   (make-paycheck String Number)
; interpretation: (make-paycheck n a) combines the name
; with the amount a

; Lop (short for list of paychecks) is one of:
; — '()
; — (cons Paycheck Lop)
; interpretation: an instance of Lop represents the
; amounts earned for a number of employees

'()
(cons (make-paycheck "Robby" 466.05)
      '())
(cons (make-paycheck "Matthew" 582.75)
      (cons (make-paycheck "Robby" 466.05)
            '()))

; Low -> Lop
; computes the list of paychecks from an-lop

(check-expect
 (wage*.v3 (cons (make-work "Robby" 11.95 39) '()))
 (cons (make-paycheck "Robby" (* 11.95 39)) '()))

(define (wage*.v3 an-lop)
  (cond
    [(empty? an-lop) '()]
    [(cons? an-lop)
     (cons (wage.v3 (first an-lop))
           (wage*.v3 (rest an-lop)))]))

; Work -> Paycheck
; computes the paycheck for the given work record w
(define (wage.v3 w)
  (make-paycheck (work-employee w)
                 (* (work-rate w) (work-hours w))))