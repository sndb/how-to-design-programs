;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex38) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; String -> String
; produces a string like the given one with the last character removed
; given: "hello", expected: "hell"
; given: "world", expected: "worl"
(define (string-remove-last s)
  (substring s 0 (sub1 (string-length s))))