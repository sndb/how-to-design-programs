;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex363) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An Xexpr is a list:
;   (cons Symbol Ending)
; where Ending is one of:
; — LXexpr
; — (cons LAttribute LXexpr)

; An LXexpr is one of:
; — '()
; — (cons Xexpr LXexpr)

; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; An LAttribute is one of:
; — '()
; — (cons Attribute LAttribute)