;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex412) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
; adds x and y

(check-expect
 (inex+ (create-inex 1 1 0) (create-inex 2 1 0))
 (create-inex 3 1 0))
(check-expect
 (inex+ (create-inex 55 1 0) (create-inex 55 1 0))
 (create-inex 11 1 1))
(check-expect
 (inex+ (create-inex 56 1 0) (create-inex 56 1 0))
 (create-inex 11 1 1))
(check-expect
 (inex+ (create-inex 1 1 0) (create-inex 1 -1 1))
 (create-inex 11 -1 1))
(check-expect
 (inex+ (create-inex 1 -1 1) (create-inex 1 1 0))
 (create-inex 11 -1 1))

(define (inex+ x y)
  (cond
    [(> (exp-diff x y) 0)
     (inex+ (exp-dec x) y)]
    [(< (exp-diff x y) 0)
     (inex+ x (exp-dec y))]
    [else
     (normalize (+ (inex-mantissa x) (inex-mantissa y))
                (inex-sign x)
                (inex-exponent x))]))

; Inex -> Inex
; decrements the exponent of x

(check-expect (exp-dec (create-inex 4 1 5))
              (create-inex 40 1 4))
(check-expect (exp-dec (create-inex 1 1 0))
              (create-inex 10 -1 1))
(check-expect (exp-dec (create-inex 1 -1 1))
              (create-inex 10 -1 2))
(check-error (exp-dec (create-inex 12 1 0))
             "cannot decrement the exponent")

(define (exp-dec x)
  (local ((define m (inex-mantissa x))
          (define s (inex-sign x))
          (define e (inex-exponent x))
          (define new-e (- e s)))
    (cond
      [(or (> m 9) (> new-e 99))
       (error "cannot decrement the exponent")]
      [(negative? new-e) (make-inex (* 10 m) (- s) (- new-e))]
      [else (make-inex (* 10 m) s new-e)])))

; N S N -> Inex
; normalizes mantissa, sign, and exponent

(check-expect (normalize 120 1 3) (create-inex 12 1 4))
(check-expect (normalize 127 1 3) (create-inex 13 1 4))
(check-expect (normalize 124 1 3) (create-inex 12 1 4))
(check-expect (normalize 955 1 3) (create-inex 96 1 4))
(check-expect (normalize 999 1 3) (create-inex 10 1 5))
(check-expect (normalize 955 -1 3) (create-inex 96 -1 2))
(check-expect (normalize 999 -1 3) (create-inex 10 -1 1))
(check-error (normalize 100 1 99) "out of range")
(check-error (normalize 100 -1 0) "out of range")

(define (normalize m s e)
  (local ((define good-exponent
            (if (= 1 s) (< e 99) (> e 0))))
    (cond
      [(and (> m 99) good-exponent)
       (normalize (round (/ m 10)) s (+ s e))]
      [(<= m 99) (make-inex m s e)]
      [else (error "out of range")])))

; Inex Inex -> Integer
; computes the difference between exponents of x and y

(check-expect
 (exp-diff (make-inex 1 1 20) (make-inex 1 -1 20))
 40)

(check-expect
 (exp-diff (make-inex 2 1 20) (make-inex 1 1 20))
 0)

(check-expect
 (exp-diff (make-inex 3 1 20) (make-inex 1 1 40))
 -20)

(define (exp-diff x y)
  (- (* (inex-sign x) (inex-exponent x))
     (* (inex-sign y) (inex-exponent y))))