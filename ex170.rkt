;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex170) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct phone [area switch four])
; A Phone is a structure:
;   (make-phone Three Three Four)
; A Three is a Number between 100 and 999.
; A Four is a Number between 1000 and 9999.

; A List-of-phones is one of:
; — '()
; — (cons Phone List-of-phones)

(define ex1 '())
(define ex2 (cons (make-phone 713 294 6492)
                  '()))
(define ex3 (cons (make-phone 462 548 1029)
                  ex2))

; List-of-phones -> List-of-phones
; replaces all occurrence of area code 713 with 281

(check-expect (replace ex1) '())
(check-expect
 (replace ex2)
 (cons (make-phone 281 294 6492)
       '()))
(check-expect
 (replace ex3)
 (cons (make-phone 462 548 1029)
       (cons (make-phone 281 294 6492)
             '())))

(define (replace l)
  (cond
    [(empty? l) '()]
    [(cons? l) (cons (replace-area (first l)) (replace (rest l)))]))

; Phone -> Phone
; replaces area code 713 with 281
(define (replace-area p)
  (make-phone (if (= 713 (phone-area p))
                  281
                  (phone-area p))
              (phone-switch p)
              (phone-four p)))