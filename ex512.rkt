;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex512) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Lam is one of: 
; – a Symbol
; – (list 'λ (list Symbol) Lam)
; – (list Lam Lam)

(define ex1 '(λ (x) x))
(define ex2 '(λ (x) y))
(define ex3 '(λ (y) (λ (x) y)))
(define ex4 '((λ (x) (x x)) (λ (x) (x x))))
(define ex5 '((λ (x) x) (λ (x) x)))
(define ex6 '(((λ (y) (λ (x) y)) (λ (z) z)) (λ (w) w)))
(define ex7 'x)

; Lam -> Boolean

(check-expect (is-var? ex1) #false)
(check-expect (is-var? ex2) #false)
(check-expect (is-var? ex3) #false)
(check-expect (is-var? ex4) #false)
(check-expect (is-var? ex5) #false)
(check-expect (is-var? ex6) #false)
(check-expect (is-var? ex7) #true)

(define (is-var? le)
  (symbol? le))

; Lam -> Boolean

(check-expect (is-λ? ex1) #true)
(check-expect (is-λ? ex2) #true)
(check-expect (is-λ? ex3) #true)
(check-expect (is-λ? ex4) #false)
(check-expect (is-λ? ex5) #false)
(check-expect (is-λ? ex6) #false)
(check-expect (is-λ? ex7) #false)

(define (is-λ? le)
  (and (list? le)
       (= 3 (length le))
       (symbol=? 'λ (first le))
       (list? (second le))
       (= 1 (length (second le)))
       (is-var? (first (second le)))
       (is-lam? (third le))))

; Lam -> Boolean

(check-expect (is-app? ex1) #false)
(check-expect (is-app? ex2) #false)
(check-expect (is-app? ex3) #false)
(check-expect (is-app? ex4) #true)
(check-expect (is-app? ex5) #true)
(check-expect (is-app? ex6) #true)
(check-expect (is-app? ex7) #false)

(define (is-app? le)
  (and (list? le)
       (= 2 (length le))
       (is-lam? (first le))
       (is-lam? (second le))))

; Lam -> Boolean

(check-expect (is-lam? ex1) #true)
(check-expect (is-lam? ex2) #true)
(check-expect (is-lam? ex3) #true)
(check-expect (is-lam? ex4) #true)
(check-expect (is-lam? ex5) #true)
(check-expect (is-lam? ex6) #true)
(check-expect (is-lam? ex7) #true)
(check-expect (is-lam? '(hello world)) #true)
(check-expect (is-lam? 1234) #false)
(check-expect (is-lam? '("hello" "world")) #false)

(define (is-lam? le)
  (or (is-var? le)
      (is-λ? le)
      (is-app? le)))

; Lam -> Symbol

(check-expect (λ-para ex1) 'x)
(check-expect (λ-para ex2) 'x)
(check-expect (λ-para ex3) 'y)

(define (λ-para le)
  (first (second le)))

; Lam -> Lam

(check-expect (λ-body ex1) 'x)
(check-expect (λ-body ex2) 'y)
(check-expect (λ-body ex3) '(λ (x) y))

(define (λ-body le)
  (third le))

; Lam -> Lam

(check-expect (app-fun ex4) '(λ (x) (x x)))
(check-expect (app-fun ex5) '(λ (x) x))
(check-expect (app-fun ex6) '((λ (y) (λ (x) y)) (λ (z) z)))

(define (app-fun le)
  (first le))

; Lam -> Lam

(check-expect (app-arg ex4) '(λ (x) (x x)))
(check-expect (app-arg ex5) '(λ (x) x))
(check-expect (app-arg ex6) '(λ (w) w))

(define (app-arg le)
  (second le))

; Lam -> [List-of Symbol]
; produces the list of all symbols used as λ parameters in a λ term

(check-expect (declareds ex1) '(x))
(check-expect (declareds ex2) '(x))
(check-expect (declareds ex3) '(y x))
(check-expect (declareds ex4) '(x x))
(check-expect (declareds ex5) '(x x))
(check-expect (declareds ex6) '(y x z w))
(check-expect (declareds ex7) '())

(define (declareds le)
  (cond
    [(is-var? le) '()]
    [(is-λ? le)
     (cons (λ-para le) (declareds (λ-body le)))]
    [(is-app? le)
     (append (declareds (app-fun le)) (declareds (app-arg le)))]))