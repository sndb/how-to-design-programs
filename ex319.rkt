;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex319) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; S-expr Symbol Symbol
; replace all occurrences of old with new

(check-expect (substitute 'hello 'hello 'world) 'world)
(check-expect (substitute '(hello 20.12 "world") 'hello 'hi)
              '(hi 20.12 "world"))
(check-expect (substitute '(wing (wing body wing) wing) 'a 'b)
              '(wing (wing body wing) wing))

(define (substitute s old new)
  (local (; Atom -> Atom
          (define (substitute-atom at)
            (cond
              [(number? at) at]
              [(string? at) at]
              [(symbol? at) (if (symbol=? at old)
                                new
                                at)]))

          ; SL -> SL
          (define (substitute-sl sl)
            (cond
              [(empty? sl) '()]
              [else
               (cons (substitute (first sl) old new)
                     (substitute-sl (rest sl)))])))
    (cond
      [(atom? s) (substitute-atom s)]
      [else (substitute-sl s)])))