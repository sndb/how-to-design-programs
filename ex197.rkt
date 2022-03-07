;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex197) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; A List-of-letters is one of:
; — '()
; — (cons Letter List-of-letters)

(define-struct lc [letter count])
; A Letter-Count is a structure:
;   (make-lc Letter Number)
; interpretation: (make-lc l c) combines the letter l with the count c

; A List-of-letter-counts is one of:
; — '()
; — (cons Letter-Count List-of-letter-counts)

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

; Dictionary List-of-letters -> List-of-letter-counts
; reports how often the given letters l occur as first ones in d

(check-expect (count-by-letter EX-DICT (list "a" "b" "z"))
              (list (make-lc "a" 3)
                    (make-lc "b" 1)
                    (make-lc "z" 0)))

(define (count-by-letter d l)
  (cond
    [(empty? l) '()]
    [(cons? l)
     (cons (make-lc (first l) (starts-with# (first l) d))
           (count-by-letter d (rest l)))]))

; Dictionary -> Letter-Count
; produces the Letter-Count for the letter that occurs most often as the first
; one in the given non-empty Dictionary

(check-expect (most-frequent (list "hello")) (make-lc "h" 1))
(check-expect (most-frequent EX-DICT) (make-lc "a" 3))

(define (most-frequent d)
  (maximum-count (count-by-letter d LETTERS)))

; List-of-letter-counts -> Letter-Count
; picks the pair with the maximum count from the non-empty list l

(check-expect (maximum-count (list (make-lc "b" 1)))
              (make-lc "b" 1))
(check-expect (maximum-count (list (make-lc "a" 3)
                                   (make-lc "b" 1)
                                   (make-lc "z" 0)))
              (make-lc "a" 3))

(define (maximum-count l)
  (cond
    [(empty? (rest l)) (first l)]
    [(cons? l)
     (if (lc>=? (first l) (maximum-count (rest l)))
         (first l)
         (maximum-count (rest l)))]))

; Letter-Count Letter-Count -> Boolean
; compares the counts of lc1 and lc2 with >=

(check-expect (lc>=? (make-lc "b" 1) (make-lc "a" 3)) #false)
(check-expect (lc>=? (make-lc "b" 1) (make-lc "z" 0)) #true)

(define (lc>=? lc1 lc2)
  (>= (lc-count lc1) (lc-count lc2)))