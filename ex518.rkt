;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex518) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct cpair [count left right])
; A [MyList X] is one of:
; – '()
; – (make-cpair (tech "N") X [MyList X])
; accumulator the count field is the number of cpairs

; data definitions, via a constructor-function 
(define (our-cons f r)
  (cond
    [(empty? r) (make-cpair 1 f r)]
    [(cpair? r) (make-cpair (+ (cpair-count r) 1) f r)]
    [else (error "our-cons: ...")]))

; Q: Argue that our-cons takes a constant amount of time to compute
; its result, regardless of the size of its input.
; A: our-cons conses f on r without traversing the latter, hence its
; complexity is O(1).