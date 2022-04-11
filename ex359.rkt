;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex359) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ERROR-VARIABLE
  "encountered a variable ")
(define ERROR-DEFINITION
  "definition not found for ")

(define-struct fun [name arg])
(define-struct add [left right])
(define-struct mul [left right])
; A BSL-fun-expr is one of: 
; – Number
; – Symbol
; — (make-fun Symbol BSL-fun-expr)
; – (make-add BSL-fun-expr BSL-fun-expr)
; – (make-mul BSL-fun-expr BSL-fun-expr)
; Examples:
(define x 5)
(define y 3)
(define expr-5 'x)
(define expr-8 (make-add 'x 3))
(define expr-7.5 (make-mul 1/2 (make-mul 'x 3)))
(define expr-34 (make-add (make-mul 'x 'x) (make-mul 'y 'y)))
(define k2 (make-fun 'k (make-add 1 1)))
(define 5k2 (make-mul 5 k2))
(define i5k2 (make-mul (make-fun 'i 5) k2))

(define-struct def [name parameter body])
; BSL-fun-def is a structure:
;   (make-def Symbol Symbol BSL-fun-expr)
; Examples:
(define fd-f (make-def 'f 'x (make-add 3 'x)))
(define fd-g (make-def 'g 'y (make-fun 'f (make-mul 2 'y))))
(define fd-h (make-def 'h 'v (make-add (make-fun 'f 'v)
                                       (make-fun 'g 'v))))

; BSL-fun-def* is a [List-of BSL-fun-def].
; Example:
(define da-fgh (list fd-f fd-g fd-h))

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
    [(fun? ex) (make-fun (fun-name ex) (subst (fun-arg ex) x v))]
    [(add? ex) (make-add (subst (add-left ex) x v)
                         (subst (add-right ex) x v))]
    [(mul? ex) (make-mul (subst (mul-left ex) x v)
                         (subst (mul-right ex) x v))]))

; BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of f in da
; signals an error if there is none

(check-expect (lookup-def da-fgh 'g) fd-g)
(check-error (lookup-def da-fgh 'a)
             (string-append ERROR-DEFINITION "a"))

(define (lookup-def da f)
  (cond
    [(empty? da) (error ERROR-DEFINITION (symbol->string f))]
    [(cons? da)
     (if (symbol=? f (def-name (first da)))
         (first da)
         (lookup-def (rest da) f))]))

; BSL-fun-expr BSL-fun-def* -> Number
; evaluates ex using da as a definition area

(check-expect (eval-function* (make-fun 'f 5)
                              da-fgh)
              8)
(check-expect (eval-function* (make-add (make-fun 'f 0)
                                        (make-fun 'g 3))
                              da-fgh)
              12)
(check-expect (eval-function* (make-mul (make-fun 'g 2)
                                        (make-fun 'h 4))
                              da-fgh)
              126)
(check-error (eval-function* (make-mul (make-fun 'g 2)
                                       'abc)
                             da-fgh)
             (string-append ERROR-VARIABLE "abc"))
(check-error (eval-function* (make-mul (make-fun 'g 2)
                                       (make-fun 'a 4))
                             da-fgh)
             (string-append ERROR-DEFINITION "a"))

(define (eval-function* ex da)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (error ERROR-VARIABLE (symbol->string ex))]
    [(fun? ex)
     (local
       (; Number
        (define arg (eval-function* (fun-arg ex) da))
        ; BSL-fun-def
        (define fn (lookup-def da (fun-name ex)))
        ; BSL-fun-expr
        (define body (subst (def-body fn)
                            (def-parameter fn)
                            arg)))
       (eval-function* body da))]
    [(add? ex)
     (+ (eval-function* (add-left ex) da)
        (eval-function* (add-right ex) da))]
    [(mul? ex)
     (* (eval-function* (mul-left ex) da)
        (eval-function* (mul-right ex) da))]))