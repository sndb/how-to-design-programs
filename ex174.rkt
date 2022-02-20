;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex174) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; A Lo1s is one of:
; — '()
; — (cons 1String Lo1s)
; interpretation: a list of 1-letter strings

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
; converts a list of lines into a \n-separated string

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
; converts a list of strings into a space-separated string
(define (join ln)
  (cond
    [(empty? ln) ""]
    [else
     (string-append (first ln)
                    (if (empty? (rest ln))
                        ""
                        " ")
                    (join (rest ln)))]))

; 1String -> String
; converts the given 1String to a 3-letter numeric String

(check-expect (encode-letter "z") (code1 "z"))
(check-expect (encode-letter "\t")
              (string-append "00" (code1 "\t")))
(check-expect (encode-letter "a")
              (string-append "0" (code1 "a")))

(define (encode-letter s)
  (cond
    [(>= (string->int s) 100) (code1 s)]
    [(< (string->int s) 10)
     (string-append "00" (code1 s))]
    [(< (string->int s) 100)
     (string-append "0" (code1 s))]))

; 1String -> String
; converts the given 1String into a String

(check-expect (code1 "z") "122")

(define (code1 c)
  (number->string (string->int c)))

; LN -> LN
; encodes a list of lines numerically

(check-expect (encode-ln '()) '())
(check-expect
 (encode-ln (cons (cons "a" (cons "b" '()))
                  (cons (cons "cd" '())
                        '())))
 (cons (cons (encode-letter "a") (cons (encode-letter "b") '()))
       (cons (cons (string-append (encode-letter "c") (encode-letter "d")) '())
             '())))

(define (encode-ln ln)
  (cond
    [(empty? ln) '()]
    [else
     (cons (encode-los (first ln)) (encode-ln (rest ln)))]))

; Los -> Los
; encodes a list of strings numerically
(define (encode-los ln)
  (cond
    [(empty? ln) '()]
    [else
     (cons (encode-string (first ln)) (encode-los (rest ln)))]))

; String -> String
; encodes a string numerically
(define (encode-string s)
  (encode-lo1s (explode s)))

; Lo1s -> String
; encodes a list of 1-letter strings numerically
(define (encode-lo1s ls)
  (cond
    [(empty? ls) ""]
    [else
     (string-append (encode-letter (first ls)) (encode-lo1s (rest ls)))]))

; String -> String
; encodes a text file n numerically
(define (encode-text-file n)
  (write-file (string-append "encoded-" n)
              (collapse
               (encode-ln
                (read-words/line n)))))