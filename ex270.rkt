;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex270) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; N -> [List-of N]
; creates the list (list 0 ... (- n 1)) for any natural number n

(check-expect (0-en 5)
              '(0 1 2 3 4))

(define (0-en n)
  (build-list n identity))

; N -> [List-of N]
; creates the list (list 1 ... n) for any natural number n

(check-expect (1-in 5)
              '(1 2 3 4 5))

(define (1-in n)
  (build-list n add1))

; N -> [List-of Number]
; creates the list (list 1 1/2 ... 1/n) for any natural number n

(check-expect (1-1/n 5)
              '(1 1/2 1/3 1/4 1/5))

(define (1-1/n n)
  (local (; N -> Number
          (define (f i)
            (/ 1 (add1 i))))
    (build-list n f)))

; N -> [List-of N]
; creates the list of the first n even numbers

(check-expect (even 5)
              '(0 2 4 6 8))

(define (even n)
  (local (; N -> N
          (define (f i)
            (* i 2)))
    (build-list n f)))

; N -> [List-of [List-of N]]
; creates a diagonal square of 0s and 1s

(check-expect (dsqr 5)
              '((1 0 0 0 0)
                (0 1 0 0 0)
                (0 0 1 0 0)
                (0 0 0 1 0)
                (0 0 0 0 1)))

(define (dsqr n)
  (local (; N -> [List-of N]
          ; creates a list of 0s with 1 at position i
          (define (f i)
            (local (; N -> N
                    (define (g j)
                      (if (= i j) 1 0)))
              (build-list n g))))
    (build-list n f)))

; [N -> Number] N -> [List-of Number]
; tabulates f between n and 0 (incl.) in a list

(check-expect (tabulate sqr 5)
              '(25 16 9 4 1 0))

(define (tabulate f n)
  (local (; N -> Number
          (define (g i)
            (f (- n i))))
    (build-list (add1 n) g)))