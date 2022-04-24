;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex413) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct inex [mantissa sign exponent])
; An Inex is a structure: 
;   (make-inex N99 S N99)
; An S is one of:
; – 1
; – -1
; An N99 is an N between 0 and 99 (inclusive).

; N Number N -> Inex
; makes an instance of Inex after checking the arguments
(define (create-inex m s e)
  (cond
    [(and (<= 0 m 99) (<= 0 e 99) (or (= s 1) (= s -1)))
     (make-inex m s e)]
    [else (error "bad values given")]))
 
; Inex -> Number
; converts an inex into its numeric equivalent 
(define (inex->number an-inex)
  (* (inex-mantissa an-inex)
     (expt
      10 (* (inex-sign an-inex) (inex-exponent an-inex)))))

(define MAX-POSITIVE (create-inex 99 1 99))
(define MIN-POSITIVE (create-inex 1 -1 99))

; Inex Inex -> Inex
; multiplies x and y

(check-expect
 (inex* (create-inex 2 1 4) (create-inex 8 1 10))
 (create-inex 16 1 14))

(check-expect
 (inex* (create-inex 20 1 1) (create-inex 5 1 4))
 (create-inex 10 1 6))

(check-expect
 (inex* (create-inex 27 -1 1) (create-inex 7 1 4))
 (create-inex 19 1 4))

(check-expect
 (inex* (create-inex 4 1 1) (create-inex 57 -1 4))
 (create-inex 23 -1 2))

(check-error
 (inex* (create-inex 20 1 99) (create-inex 20 1 99))
 "exponent overflow")

(check-error
 (inex* (create-inex 20 -1 99) (create-inex 20 -1 99))
 "exponent overflow")

(define (inex* x y)
  (local ((define m (* (inex-mantissa x) (inex-mantissa y)))
          (define e (+ (* (inex-sign x) (inex-exponent x))
                       (* (inex-sign y) (inex-exponent y))))
          (define delta (approximate m)))
    (cond
      [(> (+ (abs e) delta) 99)
       (error "exponent overflow")]
      [else
       (make-inex (round (/ m (expt 10 delta)))
                  (if (negative? e) -1 1)
                  (abs (+ e delta)))])))

; N -> N
; computes exponent delta after approximation of mantissa m

(check-expect (approximate 12) 0)
(check-expect (approximate 99) 0)
(check-expect (approximate 120) 1)
(check-expect (approximate 127) 1)
(check-expect (approximate 124) 1)
(check-expect (approximate 955) 1)
(check-expect (approximate 999) 2)
(check-expect (approximate 955) 1)

(define (approximate m)
  (cond
    [(<= 0 m 99) 0]
    [(> m 99) (+ 1 (approximate (round (/ m 10))))]))