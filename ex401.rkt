;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex401) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An S-expr (S-expression) is one of: 
; – Atom
; – [List-of S-expr]
; 
; An Atom is one of: 
; – Number
; – String
; – Symbol

; S-expr S-expr -> Boolean
; are s1 and s2 equal

(check-expect (sexp=? 4 4) #true)
(check-expect (sexp=? "hello" "hello") #true)
(check-expect (sexp=? 4 "4") #false)
(check-expect (sexp=? 4 '(4)) #false)
(check-expect (sexp=? '(4 'hello) '(4 'hello)) #true)
(check-expect (sexp=? '(4 'hello) '(4 "hello")) #false)
(check-expect (sexp=? '(4 'hello) '(4 (hello))) #false)
(check-expect (sexp=? '(4 (hello)) '(4 (hello))) #true)
(check-expect (sexp=? '(4 (hello 42)) '(4 (hello "42"))) #false)
(check-expect (sexp=? '(4 (hello 42)) '(4 (hello 42))) #true)

(define (sexp=? s1 s2)
  (cond
    [(and (number? s1) (number? s2))
     (= s1 s2)]
    [(and (string? s1) (string? s2))
     (string=? s1 s2)]
    [(and (symbol? s1) (symbol? s2))
     (symbol=? s1 s2)]
    [(and (empty? s1) (empty? s2))
     #true]
    [(and (cons? s1) (cons? s2))
     (and (sexp=? (first s1) (first s2))
          (sexp=? (rest s1) (rest s2)))]
    [else #false]))