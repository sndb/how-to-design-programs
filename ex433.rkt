;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex433) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; signals an error for those cases where the original version loops

(check-error (bundle-checked (explode "abcdefgh") 0))
(check-expect (bundle-checked '() 0) '())
(check-expect (bundle-checked (explode "abcdefgh") 2)
              (list "ab" "cd" "ef" "gh"))
(check-expect (bundle-checked (explode "abcdefg") 3)
              (list "abc" "def" "g"))
(check-expect (bundle-checked '("a" "b") 3) (list "ab"))
(check-expect (bundle-checked '() 3) '())

(define (bundle-checked s n)
  (cond
    [(and (not (empty? s)) (= 0 n))
     (error "bundle loops when n is 0 unless s is '()")]
    [else (bundle s n)]))

; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea: take n items and drop n at a time
; termination: (bundle s 0) loops unless s is '()
(define (bundle s n)
  (cond
    [(empty? s) '()]
    [else
     (cons (implode (take s n)) (bundle (drop s n) n))]))
 
; [List-of X] N -> [List-of X]
; keeps the first n items from l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))
 
; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))