;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex195) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

(define LOCATION "/usr/share/dict/words")

; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

(define EX-DICT (list "apple"
                      "animal"
                      "alpha"
                      "beta"
                      "gamma"
                      "delta"
                      "epsilon"))

; A Letter is one of the following 1Strings:
; – "a"
; – ...
; – "z"
; or, equivalently, a member? of this list:
(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))

; Letter Dictionary -> Number
; counts how many words in d start with l

(check-expect (starts-with# "a" EX-DICT) 3)
(check-expect (starts-with# "b" EX-DICT) 1)
(check-expect (starts-with# "z" EX-DICT) 0)

(define (starts-with# l d)
  (cond
    [(empty? d) 0]
    [(cons? d)
     (if (string=? l (string-ith (first d) 0))
         (add1 (starts-with# l (rest d)))
         (starts-with# l (rest d)))]))