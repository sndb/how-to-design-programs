;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex172) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; A Los is one of:
; — '()
; — (cons String Los)
; interpretation: a list of strings

; An LN is one of:
; – '()
; – (cons Los LN)
; interpretation: a list of lines, each is a list of Strings

(define line0 (cons "hello" (cons "world" '())))
(define line1 '())

(define ln0 '())
(define ln1 (cons line0 (cons line1 '())))

; LN -> String
; converts a list of lines into a string

(check-expect (collapse ln0) "")
(check-expect (collapse ln1) "hello world\n")

(define (collapse ln)
  (cond
    [(empty? ln) ""]
    [else
     (string-append (join (first ln))
                    (if (empty? (rest ln))
                        ""
                        "\n")
                    (collapse (rest ln)))]))

; Los -> String
; join a list of strings into a space-separated string
(define (join ln)
  (cond
    [(empty? ln) ""]
    [else
     (string-append (first ln)
                    (if (empty? (rest ln))
                        ""
                        " ")
                    (join (rest ln)))]))

; Los -> ???
(define (line-processor ln)
  (cond
    [(empty? ln) ...]
    [else
     (... (first ln) (line-processor (rest ln)))]))