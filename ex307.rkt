;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex307) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; String [List-of String] -> [Maybe String]
; retrieves the first string on l that is equal to, or an
; extension of, s

(check-expect (find-name "a" '("xyz" "abc" "ijk"))
              "abc")
(check-expect (find-name "ij" '("xyz" "abc" "ijk"))
              "ijk")
(check-expect (find-name "xyz" '("xyz" "abc" "ijk"))
              "xyz")
(check-expect (find-name "asdf" '("xyz" "abc" "ijk"))
              #false)

(define (find-name s l)
  (for/or ([t l])
    (if (and (<= (string-length s) (string-length t))
             (string=? s (substring t 0 (string-length s))))
        t
        #false)))

; [List-of String] N -> Boolean
; ensures that no name on l exceeds width w

(check-expect (all-not-exceeds? '("abc" "de" "f")
                                3)
              #true)
(check-expect (all-not-exceeds? '("abc" "de" "f")
                                2)
              #false)

(define (all-not-exceeds? l w)
  (for/and ([t l])
    (<= (string-length t) w)))