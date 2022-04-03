;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex328) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An S-expr is one of: 
; – Atom
; – SL

; An SL is one of: 
; – '()
; – (cons S-expr SL)
          
; An Atom is one of: 
; – Number
; – String
; – Symbol

; Any -> Boolean
; is v atom

(check-expect (atom? #false) #false)
(check-expect (atom? 'hello) #true)
(check-expect (atom? 123) #true)

(define (atom? v)
  (or (number? v)
      (string? v)
      (symbol? v)))

; S-expr Symbol Atom -> S-expr
; replaces all occurrences of old in sexp with new
 
(check-expect (substitute '(((world) bye) bye) 'bye 42)
              '(((world) 42) 42))
(check-expect (substitute 'hello 'hello 'world) 'world)
(check-expect (substitute '(hello 20.12 "world") 'hello 'hi)
              '(hi 20.12 "world"))
(check-expect (substitute '(wing (wing body wing) wing) 'a 'b)
              '(wing (wing body wing) wing))
 
(define (substitute sexp old new)
  (cond
    [(atom? sexp) (if (equal? sexp old) new sexp)]
    [else
     (map (lambda (s) (substitute s old new)) sexp)]))