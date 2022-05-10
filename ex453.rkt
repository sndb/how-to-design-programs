;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex453) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A File is one of: 
; – '()
; – (cons "\n" File)
; – (cons 1String File)
; interpretation represents the content of a file 
; "\n" is the newline character

; A Line is a [List-of 1String].

; File -> [List-of Line]
; converts a file into a list of lines

(check-expect (file->list-of-lines
               (list "a" "b" "c" "\n"
                     "d" "e" "\n"
                     "f" "g" "h" "\n"))
              (list (list "a" "b" "c")
                    (list "d" "e")
                    (list "f" "g" "h")))

(define (file->list-of-lines afile)
  (cond
    [(empty? afile) '()]
    [else
     (cons (first-line afile)
           (file->list-of-lines (remove-first-line afile)))]))
 
; File -> Line
; retrieves the prefix of afile up to the first occurrence of NEWLINE

(check-expect (first-line (list "a" "b" "c" "\n"
                                "d" "e" "\n"
                                "f" "g" "h" "\n"))
              '("a" "b" "c"))

(define (first-line afile)
  (cond
    [(empty? afile) '()]
    [(string=? (first afile) NEWLINE) '()]
    [else (cons (first afile) (first-line (rest afile)))]))
 
; File -> File
; drops the prefix of afile behind the first occurrence of NEWLINE

(check-expect (remove-first-line (list "a" "b" "c" "\n"
                                       "d" "e" "\n"
                                       "f" "g" "h" "\n"))
              '("d" "e" "\n" "f" "g" "h" "\n"))

(define (remove-first-line afile)
  (cond
    [(empty? afile) '()]
    [(string=? (first afile) NEWLINE) (rest afile)]
    [else (remove-first-line (rest afile))]))
 
(define NEWLINE "\n") ; the 1String

; A Token is one of:
; 1String[not string-whitespace?]
; String[not string-whitespace?]
; interpretation: a lower-case letter or several lower-case letters
; without whitespaces

; Line -> [List-of Token]
; turns aline into a list of tokens

(check-expect (tokenize '()) '())
(check-expect (tokenize
               (list "a" "b" "c" " "
                     "d" "e" "     "
                     "f" "g" "h" " "))
              '("abc" "de" "fgh"))
(check-expect (tokenize
               (explode "hello world"))
              '("hello" "world"))

(define (tokenize aline)
  (cond
    [(empty? aline) '()]
    [else
     (cons (first-token aline) (tokenize (remove-first-token aline)))]))

; Line -> Token
; retrieves the first token of aline behind the first occurence of a
; whitespace

(check-expect (first-token (explode "hello world"))
              "hello")

(define (first-token aline)
  (cond
    [(empty? aline) ""]
    [(string-whitespace? (first aline)) ""]
    [else
     (string-append (first aline) (first-token (rest aline)))]))

; Line -> Line
; drops the prefix of aline behind the first occurrence of a whitespace

(check-expect (remove-first-token (explode "hello world"))
              (explode "world"))

(define (remove-first-token aline)
  (cond
    [(empty? aline) '()]
    [(string-whitespace? (first aline)) (rest aline)]
    [else
     (remove-first-token (rest aline))]))