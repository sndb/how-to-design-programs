;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex397) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ERR-CANT-FIND-TIME-CARD "cannot find a time card")
(define ERR-CANT-FIND-EMPLOYEE "cannot find an employee record")

(define-struct time-card [number hours])
; A TimeCard is a structure:
;   (make-time-card Number Number)

(define-struct employee [name number pay-rate])
; An Employee is a structure:
;   (make-employee String Number Number)

(define-struct wage [name weekly])
; A Wage is a structure:
;   (make-wage String Number)

; [List-of Employee] [List-of TimeCard] -> [List-of Wage]
; produces a list of wage records
; assumption: there is at most one time card per employee number

(check-expect (wages*.v3 '() '()) '())

(check-error (wages*.v3 (list (make-employee "A" 101 24.95)
                              (make-employee "B" 102 27.45))
                        '())
             ERR-CANT-FIND-TIME-CARD)

(check-error (wages*.v3 '()
                        (list (make-time-card 101 40)
                              (make-time-card 102 35)))
             ERR-CANT-FIND-EMPLOYEE)

(check-expect (wages*.v3 (list (make-employee "A" 101 24.95)
                               (make-employee "B" 102 27.45))
                         (list (make-time-card 101 40)
                               (make-time-card 102 35)))
              (list (make-wage "A" (* 24.95 40))
                    (make-wage "B" (* 27.45 35))))

(check-expect (wages*.v3 (list (make-employee "A" 101 24.95)
                               (make-employee "B" 102 27.45))
                         (list (make-time-card 102 35)
                               (make-time-card 101 40)))
              (list (make-wage "A" (* 24.95 40))
                    (make-wage "B" (* 27.45 35))))

(define (wages*.v3 es ts)
  (cond
    [(and (empty? es) (empty? ts)) '()]
    [(and (cons? es) (empty? ts))
     (error ERR-CANT-FIND-TIME-CARD)]
    [(and (empty? es) (cons? ts))
     (error ERR-CANT-FIND-EMPLOYEE)]
    [(and (cons? es) (cons? ts))
     (cons (get-wage (first es) ts)
           (wages*.v3 (rest es) (remove-time-card ts (first es))))]))

; Employee [List-of TimeCard] -> Wage
; computes the wage of employee

(check-error (get-wage (make-employee "A" 101 24.95)
                       '())
             ERR-CANT-FIND-TIME-CARD)

(check-expect (get-wage (make-employee "A" 101 24.95)
                        (list (make-time-card 102 35)
                              (make-time-card 101 40)))
              (make-wage "A" (* 40 24.95)))

(define (get-wage e ts)
  (make-wage (employee-name e)
             (* (employee-pay-rate e)
                (hours-for (employee-number e) ts))))

; Number [List-of TimeCard] -> Number
; gets the weekly hours for employee number n from ts

(check-expect (hours-for 101
                         (list (make-time-card 102 35)
                               (make-time-card 101 40)))
              40)

(check-error (hours-for 103
                        (list (make-time-card 102 35)
                              (make-time-card 101 40)))
             ERR-CANT-FIND-TIME-CARD)

(define (hours-for n ts)
  (local ((define cards
            (filter (lambda (c)
                      (= n (time-card-number c)))
                    ts)))
    (if (= 1 (length cards))
        (time-card-hours (first cards))
        (error ERR-CANT-FIND-TIME-CARD))))

; [List-of TimeCard] Employee -> [List-of TimeCard]
; removes the time card of employee e from ts

(check-expect
 (remove-time-card '()
                   (make-employee "A" 101 24.95))
 '())

(check-expect
 (remove-time-card (list (make-time-card 102 35)
                         (make-time-card 101 40))
                   (make-employee "A" 101 24.95))
 (list (make-time-card 102 35)))

(define (remove-time-card ts e)
  (filter (lambda (c)
            (not (= (time-card-number c)
                    (employee-number e))))
          ts))