;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex175) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

(define-struct wc [letters words lines])
; A Wc is a structure:
;   (make-wc Number Number Number)
; interpretation: a number of 1Strings, words, and lines

; String -> Wc
; counts the number of 1Strings, words, and lines in a given file n
(define (wc-file n)
  (make-wc (length (read-1strings n))
           (length (read-words n))
           (length (read-lines n))))