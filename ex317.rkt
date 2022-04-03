;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex317) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; S-expr Symbol -> N 
; counts all occurrences of sy in sexp 

(check-expect (count 'world 'world) 1)
(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)

(define (count sexp sy)
  (local (; SL Symbol -> N 
          (define (count-sl sl)
            (cond
              [(empty? sl) 0]
              [else
               (+ (count (first sl) sy) (count-sl (rest sl)))]))

          ; Atom Symbol -> N 
          (define (count-atom at)
            (cond
              [(number? at) 0]
              [(string? at) 0]
              [(symbol? at) (if (symbol=? at sy) 1 0)])))
    (cond
      [(atom? sexp) (count-atom sexp)]
      [else (count-sl sexp)])))