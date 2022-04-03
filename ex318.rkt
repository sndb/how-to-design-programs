;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex318) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

'hello
20.12
"world"
'()
'(hello 20.12 "world")
'((hello 20.12 "world"))
'(define (f x)
   (+ x 55))
'((6 f)
  (5 e)
  (4 d))
'(wing (wing body wing) wing)

; Any -> Boolean
; is v atom

(check-expect (atom? #false) #false)
(check-expect (atom? 'hello) #true)
(check-expect (atom? 123) #true)

(define (atom? v)
  (or (number? v)
      (string? v)
      (symbol? v)))

; S-expr -> N
; determines sexp's depth

(check-expect (depth 'hello) 1)
(check-expect (depth '()) 1)
(check-expect (depth '((()))) 3)
(check-expect (depth '(hello 20.12 "world")) 2)
(check-expect (depth '(wing (wing body wing) wing)) 3)

(define (depth sexp)
  (local (; Atom -> N
          (define (depth-atom at)
            1)

          ; SL -> N
          (define (depth-sl sl)
            (cond
              [(empty? sl) 1]
              [else (max (add1 (depth (first sl)))
                         (depth-sl (rest sl)))])))
    (cond
      [(atom? sexp) (depth-atom sexp)]
      [else (depth-sl sexp)])))