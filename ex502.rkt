;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex502) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [NEList-of X] -> [NEList-of X]
; creates a palindrome from s0

(check-expect
 (palindrome (explode "abc")) (explode "abcba"))

(define (palindrome l0)
  (local (; [NEList-of X] [List-of X] -> [NEList-of X]
          ; accumulator a is the reverse list of items
          ; that l lacks from l0
          (define (palindrome/a l a)
            (cond
              [(empty? (rest l)) (cons (first l) a)]
              [else
               (cons (first l)
                     (palindrome/a (rest l)
                                   (cons (first l) a)))])))
    (palindrome/a l0 '())))

; [NEList-of 1String] -> [NEList-of 1String]
; creates a palindrome from s0

(check-expect
 (mirror (explode "abc")) (explode "abcba"))

(define (mirror s0)
  (append (all-but-last s0)
          (list (last s0))
          (reverse (all-but-last s0))))

; [NEList-of X] -> X
; extracts the last item from l

(check-expect (last '("a" "b")) "b")

(define (last l)
  (first (reverse l)))

; [NEList-of X] -> [List-of X]
; extracts all the items except the last one

(check-expect (all-but-last '("a")) '())
(check-expect (all-but-last '("a" "b")) '("a"))

(define (all-but-last l)
  (cond
    [(empty? (rest l)) '()]
    [else
     (cons (first l) (all-but-last (rest l)))]))