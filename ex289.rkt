;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex289) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; String [List-of String] -> Boolean
; determines whether any of the names on l are equal to or
; an extension of s

(check-expect (find-name "abc"
                         '("abcd"
                           "ijk"
                           "xyz"))
              #true)
(check-expect (find-name "abc"
                         '("ijk"
                           "xyz"))
              #false)

(define (find-name s l)
  (ormap (lambda (t)
           (if (> (string-length t) (string-length s))
               (string=? (substring t 0 (string-length s)))
               (string=? s t)))
         l))

; [List-of String] -> Boolean
; checks if all names on l start with the letter "a"

(check-expect (all-start-with-a? '("abc" "def" "ghi"))
              #false)
(check-expect (all-start-with-a? '("abc" "ace" "adg"))
              #true)
(check-expect (all-start-with-a? '(""))
              #false)

(define (all-start-with-a? l)
  (andmap (lambda (s) (and (positive? (string-length s))
                           (string=? "a" (string-ith s 0))))
          l))

; [List-of String] N -> Boolean
; checks if no name on l exceeds a given width n

(check-expect (all-not-exceeds? '("abc" "de" "f")
                                3)
              #true)
(check-expect (all-not-exceeds? '("abc" "de" "f")
                                2)
              #false)

(define (all-not-exceeds? l n)
  (andmap (lambda (s) (<= (string-length s) n))
          l))