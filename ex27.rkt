;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex27) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PRICE 5.0)
(define ATTENDANCE 120)
(define PRICE-CHANGE 0.1)
(define ATTENDANCE-CHANGE 15)
(define COST 180)
(define COST-PER-ATTENDEE 0.04)

(define (attendees ticket-price)
  (- ATTENDANCE (* (- ticket-price PRICE) (/ ATTENDANCE-CHANGE PRICE-CHANGE))))

(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

(define (cost ticket-price)
  (+ COST (* COST-PER-ATTENDEE (attendees ticket-price))))

(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))