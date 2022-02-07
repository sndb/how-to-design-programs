;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex124) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PRICE 5)
(define SALES-TAX (* 0.08 PRICE))
(define TOTAL (+ PRICE SALES-TAX))
; ==
;(define PRICE 5)
;(define SALES-TAX (* 0.08 5))
;(define TOTAL (+ PRICE SALES-TAX))
; ==
;(define PRICE 5)
;(define SALES-TAX 0.4)
;(define TOTAL (+ PRICE SALES-TAX))
; ==
;(define PRICE 5)
;(define SALES-TAX 0.4)
;(define TOTAL (+ 5 SALES-TAX))
; ==
;(define PRICE 5)
;(define SALES-TAX 0.4)
;(define TOTAL (+ 5 0.4))
; ==
;(define PRICE 5)
;(define SALES-TAX 0.4)
;(define TOTAL 5.4)

;(define COLD-F 32)
;(define COLD-C (fahrenheit->celsius COLD-F))
;(define (fahrenheit->celsius f)
;  (* 5/9 (- f 32)))
; signals an error: fahrenheit->celsius is used here before its definition

(define LEFT -100)
(define RIGHT 100)
(define (f x) (+ (* 5 (expt x 2)) 10))
(define f@LEFT (f LEFT))
(define f@RIGHT (f RIGHT))
;==
;(define LEFT -100)
;(define RIGHT 100)
;(define (f x) (+ (* 5 (expt x 2)) 10))
;(define f@LEFT (f -100))
;(define f@RIGHT (f RIGHT))
;==
;(define LEFT -100)
;(define RIGHT 100)
;(define (f x) (+ (* 5 (expt x 2)) 10))
;(define f@LEFT (+ (* 5 (expt -100 2)) 10))
;(define f@RIGHT (f RIGHT))
;==
;(define LEFT -100)
;(define RIGHT 100)
;(define (f x) (+ (* 5 (expt x 2)) 10))
;(define f@LEFT (+ (* 5 10000) 10))
;(define f@RIGHT (f RIGHT))
;==
;(define LEFT -100)
;(define RIGHT 100)
;(define (f x) (+ (* 5 (expt x 2)) 10))
;(define f@LEFT (+ 50000 10))
;(define f@RIGHT (f RIGHT))
;==
;(define LEFT -100)
;(define RIGHT 100)
;(define (f x) (+ (* 5 (expt x 2)) 10))
;(define f@LEFT 50010)
;(define f@RIGHT (f RIGHT))
;==
;(define LEFT -100)
;(define RIGHT 100)
;(define (f x) (+ (* 5 (expt x 2)) 10))
;(define f@LEFT 50010)
;(define f@RIGHT (f 100))
;==
;(define LEFT -100)
;(define RIGHT 100)
;(define (f x) (+ (* 5 (expt x 2)) 10))
;(define f@LEFT 50010)
;(define f@RIGHT (+ (* 5 (expt 100 2)) 10))
;==
;(define LEFT -100)
;(define RIGHT 100)
;(define (f x) (+ (* 5 (expt x 2)) 10))
;(define f@LEFT 50010)
;(define f@RIGHT (+ (* 5 10000) 10))
;==
;(define LEFT -100)
;(define RIGHT 100)
;(define (f x) (+ (* 5 (expt x 2)) 10))
;(define f@LEFT 50010)
;(define f@RIGHT (+ 50000 10))
;==
;(define LEFT -100)
;(define RIGHT 100)
;(define (f x) (+ (* 5 (expt x 2)) 10))
;(define f@LEFT 50010)
;(define f@RIGHT 50010)