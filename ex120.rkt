;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex120) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(x)
; illegal; is not a def-expr

(+ 1 (not x))
; legal;
; is an expr: (name expr expr ...), because
; (not x) is a (name expr expr ...), which is an expr

(+ 1 2 3)
; legal;
; is an expr: (name expr expr ...), because
; 1, 2, and 3 are numbers, which belong to exprs