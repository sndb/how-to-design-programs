;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex400) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A DNA-Letter is one of 'a, 'c, 'g, or 't.

; [List-of DNA-Letter] [List-of DNA-Letter] -> Boolean
; #true if pattern is identical to the initial part of s
; #false otherwise

(check-expect (DNAprefix '() '()) #true)
(check-expect (DNAprefix '() '(a g t c)) #true)
(check-expect (DNAprefix '(a g) '()) #false)
(check-expect (DNAprefix '(a g) '(a g t c)) #true)

(define (DNAprefix pattern s)
  (cond
    [(empty? pattern) #true]
    [(empty? s) #false]
    [(cons? s)
     (and (symbol=? (first pattern) (first s))
          (DNAprefix (rest pattern) (rest s)))]))

; [List-of DNA-Letter] [List-of DNA-Letter] -> [Maybe DNA-Letter]
; returns the first item in s beyond pattern

(check-error (DNAdelta '() '()))
(check-expect (DNAdelta '() '(a g t c)) 'a)
(check-expect (DNAdelta '(a g) '()) #false)
(check-expect (DNAdelta '(a g) '(a g t c)) 't)
(check-expect (DNAdelta '(a t) '(a g t c)) #false)
(check-error (DNAdelta '(a g t c) '(a g t c)))

(define (DNAdelta pattern s)
  (cond
    [(and (empty? pattern) (empty? s))
     (error "no DNA letter beyond pattern")]
    [(empty? pattern) (first s)]
    [(empty? s) #false]
    [(cons? s)
     (if (symbol=? (first pattern) (first s))
         (DNAdelta (rest pattern) (rest s))
         #false)]))