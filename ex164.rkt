;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex164) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define US-TO-EUR-RATE 0.88)

; A List-of-numbers is one of:
; – '()
; – (cons Number List-of-numbers)

; Number -> Number
; converts a US$ amount into a € amounts

(check-expect (us-to-eur 42) (* US-TO-EUR-RATE 42))

(define (us-to-eur n)
  (* US-TO-EUR-RATE n))

; List-of-numbers -> List-of-numbers
; converts a list of US$ amounts into a list of € amounts

(check-expect (convert-euro '()) '())
(check-expect (convert-euro (cons 10 '()))
              (cons (us-to-eur 10) '()))
(check-expect (convert-euro (cons 42 (cons 10 '())))
              (cons (us-to-eur 42) (cons (us-to-eur 10) '())))

(define (convert-euro l)
  (cond
    [(empty? l) '()]
    [else
     (cons (us-to-eur (first l))
           (convert-euro (rest l)))]))

; Number List-of-numbers -> List-of-numbers
; converts a list of US$ amounts into a list of € amounts
; according to an exchange-rate

(check-expect (convert-euro* 0.9 '()) '())
(check-expect (convert-euro* 0.9 (cons 10 '()))
              (cons (* 0.9 10) '()))
(check-expect (convert-euro* 0.9 (cons 42 (cons 10 '())))
              (cons (* 0.9 42) (cons (* 0.9 10) '())))

(define (convert-euro* xr l)
  (cond
    [(empty? l) '()]
    [else
     (cons (* xr (first l)) (convert-euro* xr (rest l)))]))