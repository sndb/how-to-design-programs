;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex510) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; N String String -> String
; rearrange the words in in-f so that all lines in the resulting file
; out-f have a maximal width of w
(define (fmt w in-f out-f)
  (write-file out-f
              (foldr (lambda (line lines)
                       (string-append line "\n" lines))
                     ""
                     (fmt-words w (read-words in-f)))))

; N [List-of String] -> [List-of String]
; rearrange words0 into the lines that have a maximal width of w

(check-expect (fmt-words 10 '("abc" "def" "ghi" "jkl"))
              '("abc def" "ghi jkl"))
(check-expect (fmt-words 11 '("abc" "def" "ghi" "jkl"))
              '("abc def ghi" "jkl"))

(define (fmt-words w words0)
  (local (; N [List-of String] [List-of String] -> [List-of String]
          ; accumulator a is the list of words that words lacks from
          ; words0 arranged in lines of width w
          (define (fmt-words/a words a)
            (cond
              [(empty? words) (reverse a)]
              [else
               (local ((define last-line
                         (if (cons? a) (first a) "")))
                 (if (<= (+ 1
                            (string-length (first words))
                            (string-length last-line))
                         w)
                     (fmt-words/a (rest words)
                                  (append-to-first-string
                                   a (first words)))
                     (fmt-words/a words (cons "" a))))])))
    (fmt-words/a words0 '())))

; [List-of String] String -> [List-of String]
; appends s to the first item on l

(check-expect (append-to-first-string '() "hello") '("hello"))
(check-expect (append-to-first-string '("hello" "world") "123")
              '("hello 123" "world"))

(define (append-to-first-string l s)
  (cond
    [(empty? l) (list s)]
    [else
     (cons
      (string-append (first l)
                     (if (string=? "" (first l)) "" " ") s)
      (rest l))]))