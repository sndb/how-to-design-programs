;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex354) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A BSL-expr is one of:
; — Value
; — Addition
; — Multiplication

; A Value is a Number.

(define-struct add [left right])
; An Addition is a structure:
;   (make-add BSL-expr BSL-expr)
; interpretation: (make-add 2 2) represents an expression (+ 2 2)

(define-struct mul [left right])
; A Multiplication is a structure:
;   (make-mul BSL-expr BSL-expr)
; interpretation: (make-mul 2 2) represents an expression (+ 2 2)

; BSL-expr examples
(define expr-3 3)
(define expr-2 (make-add 1 1))
(define expr-30 (make-mul 3 10))
(define expr-11 (make-add (make-mul 1 1) 10))

; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

; BSL-var-expr examples
(define x 5)
(define y 3)
(define expr-5
  'x)
(define expr-8
  (make-add 'x 3))
(define expr-7.5
  (make-mul 1/2 (make-mul 'x 3)))
(define expr-34
  (make-add (make-mul 'x 'x)
            (make-mul 'y 'y)))

; An AL (short for association list) is [List-of Association].
; An Association is a list of two items:
;   (cons Symbol (cons Number '())).

; Association examples
(define as-x5 '(x 5))
(define as-y3 '(y 3))

; AL examples
(define al-x5 (list as-x5))
(define al-x5y3 (list as-x5 as-y3))

; BSL-var-expr AL -> Number
; determines the value of ex if numeric using da as a
; definition area; error otherwise

(check-expect (eval-variable* expr-30 '()) 30)
(check-expect (eval-variable* expr-11 '()) 11)
(check-error (eval-variable* expr-5 '()) NOT-NUMERIC)
(check-expect (eval-variable* expr-7.5 al-x5) 7.5)
(check-error (eval-variable* expr-34 al-x5) NOT-NUMERIC)
(check-expect (eval-variable* expr-5 al-x5y3) 5)
(check-expect (eval-variable* expr-7.5 al-x5y3) 7.5)
(check-expect (eval-variable* expr-34 al-x5y3) 34)

(define (eval-variable* ex da)
  (eval-variable
   (foldr (lambda (as ex)
            (subst ex (first as) (second as)))
          ex da)))

; BSL-var-expr -> Number
; determines the value of ex if numeric; error otherwise

(check-expect (eval-variable expr-30) 30)
(check-expect (eval-variable expr-11) 11)
(check-error (eval-variable expr-5) NOT-NUMERIC)
(check-error (eval-variable expr-7.5) NOT-NUMERIC)

(define (eval-variable ex)
  (if (numeric? ex)
      (eval-expression ex)
      (error NOT-NUMERIC)))

(define NOT-NUMERIC "eval-variable: not numeric")

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

; BSL-var-expr -> Boolean
; determines whether ex is a BSL-expr

(check-expect (numeric? expr-30) #true)
(check-expect (numeric? expr-11) #true)
(check-expect (numeric? expr-5) #false)
(check-expect (numeric? expr-7.5) #false)

(define (numeric? ex)
  (cond
    [(number? ex) #true]
    [(symbol? ex) #false]
    [(add? ex) (and (numeric? (add-left ex))
                    (numeric? (add-right ex)))]
    [(mul? ex) (and (numeric? (mul-left ex))
                    (numeric? (mul-right ex)))]))

; BSL-var-expr Symbol Number -> BSL-var-expr
; replace all occurrences of x by v

(check-expect (subst expr-5 'x 3) 3)
(check-expect (subst expr-8 'x 10)
              (make-add 10 3))
(check-expect (subst expr-7.5 'x 7)
              (make-mul 1/2 (make-mul 7 3)))
(check-expect (subst expr-34 'y 2)
              (make-add (make-mul 'x 'x)
                        (make-mul 2 2)))

(define (subst ex x v)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (if (symbol=? ex x) v ex)]
    [(add? ex) (make-add (subst (add-left ex) x v)
                         (subst (add-right ex) x v))]
    [(mul? ex) (make-mul (subst (mul-left ex) x v)
                         (subst (mul-right ex) x v))]))