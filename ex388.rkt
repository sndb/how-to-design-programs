;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex388) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct employee [name ssn pay-rate])
(define-struct record [name hours])
(define-struct wage [name pay])

; An Employee is a structure:
;   (make-employee String Number Number)
; interpretation: represents an employee's name, social
; security number, and pay rate.

; A Record is a structure:
;   (make-record String Number)
; interpretation: represents an employee's name and the
; number of hours worked in a week.

; A Wage is a structure:
;   (make-wage String Number)
; interpretation: represents the name of the employee and
; the weekly wage.

; [List-of Employee] [List-of Record] -> [List-of Wage]
; computes weekly wages
; assume the two lists are of equal length

(check-expect (wages*.v2 '() '()) '())
(check-expect (wages*.v2 (list (make-employee "A" 101 5.65))
                         (list (make-record "A" 40)))
              (list (make-wage "A" 226.0)))
(check-expect (wages*.v2 (list (make-employee "A" 101 5.65)
                               (make-employee "B" 102 8.75))
                         (list (make-record "A" 40)
                               (make-record "B" 30)))
              (list (make-wage "A" 226.0)
                    (make-wage "B" 262.5)))

(define (wages*.v2 es rs)
  (cond
    [(empty? es) '()]
    [else
     (cons
      (weekly-wage (first es) (first rs))
      (wages*.v2 (rest es) (rest rs)))]))

; Employee Record -> Wage
; computes the weekly wage
(define (weekly-wage e r)
  (make-wage (employee-name e)
             (* (employee-pay-rate e) (record-hours r))))