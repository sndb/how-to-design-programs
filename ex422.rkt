;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex422) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea take n items and drop n at a time

(check-expect (bundle (explode "abcdefgh") 2)
              (list "ab" "cd" "ef" "gh"))
(check-expect (bundle (explode "abcdefg") 3)
              (list "abc" "def" "g"))
(check-expect (bundle '("a" "b") 3) (list "ab"))
(check-expect (bundle '() 3) '())

(define (bundle s n)
  (map implode (list->chunks s n)))

; [List-of X] N -> [List-of [List-of X]]
; produces a list of list chunks of size n

(check-expect (list->chunks (explode "abcdefgh") 2)
              '(("a" "b") ("c" "d") ("e" "f") ("g" "h")))
(check-expect (list->chunks (explode "abcdefg") 3)
              `(,(explode "abc") ,(explode "def") ("g")))
(check-expect (list->chunks '("a" "b") 3) '(("a" "b")))
(check-expect (list->chunks '() 3) '())

(define (list->chunks l n)
  (cond
    [(empty? l) '()]
    [else
     (cons (take l n) (list->chunks (drop l n) n))]))
 
; [List-of X] N -> [List-of X]
; keeps the first n items from l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))
 
; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))