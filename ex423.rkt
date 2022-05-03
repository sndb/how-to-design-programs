;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex423) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; String N -> [List-of String]
; ; produces a list of string chunks of size n

(check-expect (partition "abcdefgh" 2)
              (list "ab" "cd" "ef" "gh"))
(check-expect (partition "abcdefg" 3)
              (list "abc" "def" "g"))
(check-expect (partition "ab" 3) (list "ab"))
(check-expect (partition "" 3) '(""))

(define (partition s n)
  (cond
    [(<= (string-length s) n) (list s)]
    [else
     (cons (substring s 0 n)
           (partition (substring s n) n))]))