;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex357) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct fun [name arg])
(define-struct add [left right])
(define-struct mul [left right])
; A BSL-fun-expr is one of: 
; – Number
; – Symbol
; — (make-fun Symbol BSL-fun-expr)
; – (make-add BSL-fun-expr BSL-fun-expr)
; – (make-mul BSL-fun-expr BSL-fun-expr)

; BSL-fun-expr examples
;(k (+ 1 1))
(define k2 (make-fun 'k (make-add 1 1)))
;(* 5 (k (+ 1 1)))
(define 5k2 (make-mul 5 k2))
;(* (i 5) (k (+ 1 1)))
(define i5k2 (make-mul (make-fun 'i 5) k2))

; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

; BSL-var-expr examples
(define x 5)
(define y 3)
(define expr-5 'x)
(define expr-8 (make-add 'x 3))
(define expr-7.5 (make-mul 1/2 (make-mul 'x 3)))
(define expr-34 (make-add (make-mul 'x 'x) (make-mul 'y 'y)))

; BSL-fun-expr Symbol Symbol BSL-fun-expr -> Number
; determines the value of ex

(check-expect (eval-definition1 k2 'k 'v (make-mul 'v 'v))
              4)
(check-expect (eval-definition1 5k2 'k 'v (make-add 1 'v))
              15)
(check-error (eval-definition1 5k2 'k 'v (make-add 'a 'v))
             (string-append ERROR-VARIABLE "a"))
(check-error (eval-definition1 i5k2 'k 'v (make-add '1 'v))
             (string-append ERROR-FUNCTION "k"))

(define (eval-definition1 ex f x b)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (error ERROR-VARIABLE (symbol->string ex))]
    [(fun? ex)
     (if (symbol=? f (fun-name ex))
         (local ((define value
                   (eval-definition1 (fun-arg ex) f x b))
                 (define plugd
                   (subst b x value)))
           (eval-definition1 plugd f x b))
         (error ERROR-FUNCTION (symbol->string f)))]
    [(add? ex)
     (+ (eval-definition1 (add-left ex) f x b)
        (eval-definition1 (add-right ex) f x b))]
    [(mul? ex)
     (* (eval-definition1 (mul-left ex) f x b)
        (eval-definition1 (mul-right ex) f x b))]))

(define ERROR-FUNCTION
  "function application refer to a function name other than ")
(define ERROR-VARIABLE
  "encountered a variable ")

; BSL-fun-expr Symbol Number -> BSL-fun-expr
; replace all occurrences of x by v

(check-expect (subst expr-5 'x 3) 3)
(check-expect (subst expr-8 'x 10)
              (make-add 10 3))
(check-expect (subst expr-7.5 'x 7)
              (make-mul 1/2 (make-mul 7 3)))
(check-expect (subst expr-34 'y 2)
              (make-add (make-mul 'x 'x)
                        (make-mul 2 2)))
(check-expect (subst k2 'x 1) k2)

(define (subst ex x v)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (if (symbol=? ex x) v ex)]
    [(fun? ex) ex]
    [(add? ex) (make-add (subst (add-left ex) x v)
                         (subst (add-right ex) x v))]
    [(mul? ex) (make-mul (subst (mul-left ex) x v)
                         (subst (mul-right ex) x v))]))

; will run forever
;(eval-definition1 (make-fun 'f 1) 'f 'v (make-fun 'f 1))