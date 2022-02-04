;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex72) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct phone [area number])
; A Phone is a structure:
;   (make-phone Number String)
; interpretation: an area code and a local number
; examples:
;   (make-phone 207 "363-2421")
;   (make-phone 101 "776-1099")
;   (make-phone 208 "112-9981")

(define-struct phone# [area switch num])
; A Phone# is a structure:
;   (make-phone# Number Number Number)
; interpretation:
;   an area code, in the interval [000, 999],
;   a phone switch code, in the interval [000, 999], and
;   a local number, in the interval [0000, 9999]