;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex351) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define WRONG "the given S-expr has no BSL-expr representative")

; A BSL-expr is one of:
; — Addition
; — Multiplication
; — Value

(define-struct add [left right])
; An Addition is a structure:
;   (make-add BSL-expr BSL-expr)
; interpretation: (make-add 2 2) represents an expression (+ 2 2)

(define-struct mul [left right])
; A Multiplication is a structure:
;   (make-mul BSL-expr BSL-expr)
; interpretation: (make-mul 2 2) represents an expression (+ 2 2)

; A Value is a Number.

; Examples
(define expr-3 3)
(define expr-2 (make-add 1 1))
(define expr-30 (make-mul 3 10))
(define expr-11 (make-add (make-mul 1 1) 10))

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

; S-expr -> Value
; produces the value of s

(check-expect (interpreter-expr 3) 3)
(check-expect (interpreter-expr '(+ 1 1)) 2)
(check-expect (interpreter-expr '(* 3 10)) 30)
(check-expect (interpreter-expr '(+ (* 1 1) 10)) 11)
(check-error (interpreter-expr '(/ 4 2)) WRONG)
(check-error (interpreter-expr '(+ 1 2 3)) WRONG)
(check-error (interpreter-expr "hello") WRONG)
(check-error (interpreter-expr 'hello) WRONG)

(define (interpreter-expr s)
  (eval-expression (parse s)))

; BSL-expr -> Value
; computes the value of expr

(check-expect (eval-expression expr-3) 3)
(check-expect (eval-expression expr-2) 2)
(check-expect (eval-expression expr-30) 30)
(check-expect (eval-expression expr-11) 11)

(define (eval-expression e)
  (cond
    [(add? e)
     (+ (eval-expression (add-left e))
        (eval-expression (add-right e)))]
    [(mul? e)
     (* (eval-expression (mul-left e))
        (eval-expression (mul-right e)))]
    [else e]))

; S-expr -> BSL-expr

(check-expect (parse 3) expr-3)
(check-expect (parse '(+ 1 1)) expr-2)
(check-expect (parse '(* 3 10)) expr-30)
(check-expect (parse '(+ (* 1 1) 10)) expr-11)
(check-error (parse '(/ 4 2)) WRONG)
(check-error (parse '(+ 1 2 3)) WRONG)
(check-error (parse "hello") WRONG)
(check-error (parse 'hello) WRONG)

(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))
 
; SL -> BSL-expr 
(define (parse-sl s)
  (cond
    [(and (consists-of-3 s) (symbol? (first s)))
     (cond
       [(symbol=? (first s) '+)
        (make-add (parse (second s)) (parse (third s)))]
       [(symbol=? (first s) '*)
        (make-mul (parse (second s)) (parse (third s)))]
       [else (error WRONG)])]
    [else (error WRONG)]))
 
; Atom -> BSL-expr 
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))
 
; SL -> Boolean
(define (consists-of-3 s)
  (and (cons? s) (cons? (rest s)) (cons? (rest (rest s)))
       (empty? (rest (rest (rest s))))))

; Any -> Boolean
; is v atom
(define (atom? v)
  (or (number? v)
      (string? v)
      (symbol? v)))