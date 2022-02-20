;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex173) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

(define ARTICLES (cons "a" (cons "an" (cons "the" '()))))

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

; LN -> LN
; removes all articles from ln

(check-expect (remove-articles '()) '())
(check-expect
 (remove-articles (cons (cons "a" '())
                        (cons (cons "an" '())
                              (cons (cons "the" '())
                                    '()))))
 (cons '()
       (cons '()
             (cons '()
                   '()))))
(check-expect
 (remove-articles (cons (cons "a"
                              (cons "b"
                                    (cons "c"
                                          '())))
                        '()))
 (cons (cons "b"
             (cons "c"
                   '()))
       '()))

(define (remove-articles ln)
  (cond
    [(empty? ln) '()]
    [else
     (cons (remove-articles-from-line (first ln))
           (remove-articles (rest ln)))]))

; Los -> Los
; removes all articles from ln

(check-expect (remove-articles-from-line '()) '())
(check-expect
 (remove-articles-from-line (cons "a"
                                  (cons "an"
                                        (cons "the"
                                              '()))))
 '())
(check-expect
 (remove-articles-from-line (cons "a"
                                  (cons "b"
                                        (cons "c"
                                              '()))))
 (cons "b" (cons "c" '())))

(define (remove-articles-from-line ln)
  (cond
    [(empty? ln) '()]
    [else
     (if (member? (first ln) ARTICLES)
         (remove-articles-from-line (rest ln))
         (cons (first ln) (remove-articles-from-line (rest ln))))]))

; String -> String
; removes all articles from a text file n
; writes the result to a text file no-articles-n
(define (remove-articles-from-file n)
  (write-file (string-append "no-articles-" n)
              (collapse
               (remove-articles
                (read-words/line n)))))